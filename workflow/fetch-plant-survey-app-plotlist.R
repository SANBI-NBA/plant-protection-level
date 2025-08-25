# ------------------------------------------------------------
# Fetch Cloudant “plotsubmission” documents and flatten them
# into a dataframe. The data is then processed to fit the 
# geospeciesoccurrences table in the plant protection level 
# assessment database. For species with enough presences in plots
# densities are also calculated for use in setting species targets
# ------------------------------------------------------------

# ---- packages ------------------------------------------------
suppressPackageStartupMessages({
  library(httr)      # HTTP
  library(jsonlite)  # JSON ↔ R
  library(readr)     # Fast CSV writer
  library(dplyr)     # For bind_rows (optional but handy)
  library(dbx)       # For postgres database connection
  library(DBI)       # For interacting with postgres data
})

# ---- set up assessment variables -----------------------------
# These variables are applied in various parts of the code below
# To ensure that data is attributed to the correct protection level assessment
# Make sure these are set up correctly for the current assessment before running the code below

var_assessment_id <- 2
var_assessment_year <- 2025


# ---- request data from app -------------------------------------------------

# The URL for the App backend is stored in a variable in .Renviron
url <- Sys.getenv(PLANT_SURVEY_APP_URL)

payload <- list(
  selector       = list(type = "plotsubmission"),
  execution_stats = TRUE
)

resp <- POST(
  url,
  body   = payload,
  encode = "json",
  timeout(60)
)

stop_for_status(resp)

docs <- content(resp, as = "parsed", simplifyVector = FALSE)$docs

# ---- transform to df ----------------------------------------------
rows <- vector("list", length = 0)

for (doc in docs) {
  # Common (plot-level) fields
  common <- list(
    plotId             = doc[["_id"]],
    version            = doc[["_rev"]],
    gridCode           = doc[["gridCode"]],
    latitude           = doc[["latitude"]],
    longitude          = doc[["longitude"]],
    positionAccuracy   = doc[["positionAccuracy"]],
    altitude           = doc[["altitude"]],
    localityDescription = doc[["localityDescription"]],
    habitatDescription  = doc[["habitatDescription"]],
    siteCondition      = doc[["siteCondition"]],
    areaSampled        = doc[["areaSampled"]],
    date               = doc[["date"]],
    surveyorName       = doc[["surveyorName"]]
  )

  # Add a row for every entry in plotList (species level)
  plot_list <- doc[["plotList"]]
  if (length(plot_list) == 0) {
    # Still write a row with species fields blank
    rows[[length(rows) + 1]] <-
      c(common,
        list(speciesId = NA, speciesName = NA, count = NA))
  } else {
    for (plot in plot_list) {
      rows[[length(rows) + 1]] <-
        c(common,
          list(
            speciesId   = plot[["speciesId"]],
            speciesName = plot[["speciesName"]],
            count       = plot[["count"]]
          ))
    }
  }
}

df <- bind_rows(rows)

# ---- process presence records for geospeciesoccurrences -------------

# filter out test data
dat <- df %>% filter(!grepl('Rudi|Starke', surveyorName))

# write data snapshot for assessment reference
# this needs to be stored in the NBA data repository

write.csv(dat, file = paste0("plant-survey-app-data-", var_assessment_year, ".csv"), row.names = FALSE, na = "")

# keep only presence records
dat <- dat %>% filter(count != 0)

# process and select required columns

dat <- dat %>% mutate(precisionm = as.integer(if_else(positionAccuracy < 100, 100, round(positionAccuracy))),
                      locality_description = paste(localityDescription, habitatDescription, sep = " - "),
                      collection_year = as.integer(lubridate::year(date)),
                      locality_source = "Plant Survey App",
                      min_count = count,
                      max_count = count) %>% 
               select(speciesId, speciesName, latitude, longitude, precisionm, locality_description,
                      surveyorName, collection_year, locality_source, min_count, max_count)

# ---- postgres database connection ---------------------------

# Note that if the database is in a docker container, the container needs to be running
# It also requires the db-connection.R script and an .Renviron file containing the database login info
# Remember to close the database connection when done

source("db-connection.R")

speciesdat <- dbGetQuery(db, "SELECT species_id, rl_id, taxon, current_name, new_id FROM speciesdata.tblspecies")

# Join speciesids to plant survey app data
dat <- left_join(dat, speciesdat, by = c('speciesId' = 'rl_id'))

# Fix outdated taxonomy
dat <- dat %>% mutate(species_id = if_else(current_name == 0, new_id, species_id))

# The App also collects data on species of conservation concern that are not part of the sample
# At this stage these species would have NULLs for IDs, and they need to be filtered out of the list
dat <- dat %>% filter(!is.na(species_id))

# Assign assessment id : this is used to identify the occurrence records used in each protection level assessment.
# This number should be present in the table refassessment
dat <- dat %>% mutate(assessment_id = as.integer(var_assessment_id),
                      qc = as.integer(0))

# Final fixes on the df to make sure columns and column names match geospeciesoccurrences exactly
dat <- dat %>% select(assessment_id, species_id, latitude, longitude, precisionm, locality_description, surveyorName,
                      collection_year, locality_source, min_count, max_count, qc) %>% 
               rename(collector = surveyorName)

# Insert the data into geospeciesoccurrences. 
# First we make a temporary table in the database:
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpoccurrences"), value = dat, overwrite = TRUE)

# Check this table in the database to see that everything transferred correctly, geometries are valid, etc.
# Then run this code to move the records to geospeciesoccurrences

dbExecute(db, "
  insert into speciesdata.geospeciesoccurrences (
    assessment_id,
	  species_id,
	  latitude,
	  longitude,
	  precisionm,
	  locality_description,
	  collector,
	  collection_year,
	  locality_source,
	  min_count,
	  max_count,
	  geom)
select 
    assessment_id,
    species_id,
	  latitude,
	  longitude,
	  precisionm,
	  locality_description,
	  collector,
	  collection_year,
	  locality_source,
	  min_count,
	  max_count,
	  ST_SetSRID(ST_MakePoint(longitude, latitude), 4326) 
from speciesdata.tmpoccurrences
")

# Delete the temporary table
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpoccurrences")

# ---- calculate densities ---------------------------

# The plant survey app is set up specifically to estimate species densities in the field.
# This data is used to estimate the number of individuals in a protected area based on intersects
# of suitable habitat maps with protected areas

# Start again with data pulled from app, filter out test data
densdat <- df %>% filter(!grepl('Rudi|Starke', surveyorName))

# Group and summarise data by species
densdat <- densdat %>% group_by(speciesId, speciesName) %>% 
                        summarise(sum_area = sum(areaSampled),
                                  sum_count = sum(count),
                                  plot_count = n())

# Remove all species not yet recorded present in plots (density will be 0)
densdat <- densdat %>% filter(sum_count != 0)

# Calculate density
densdat <- densdat %>% mutate(density = sum_count/(sum_area*0.0001)) # converting square meters to hectares because density is recorded as plants per hectare

# Get correct speciesids and fix taxonomy
densdat <- left_join(densdat, speciesdat, by = c('speciesId' = 'rl_id'))
densdat <- densdat %>% mutate(species_id = if_else(current_name == 0, new_id, species_id))
densdat <- densdat %>% filter(!is.na(species_id))

# Wrangle data to match tblspeciesdensity in database
densdat <- densdat %>% ungroup() %>% 
                       select(species_id, density) %>% 
                       mutate(density_source = "Plant Survey App",
                              date_calculated = Sys.Date(),
                              assessments = as.integer(var_assessment_id))

# Pass data to temporary table in database
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "newdensities"), value = densdat, overwrite = TRUE)

# Append the data to tblspeciesdensity

dbExecute(db, "
  insert into speciesdata.tblspeciesdensity (
    species_id,
    density,
    density_source,
    date_calculated,
    assessments)
  select 
    species_id,
    density,
    density_source,
    date_calculated,
    array[assessments]
  from speciesdata.newdensities;
")

# Remove temporary table from database
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.newdensities")

# ---- close database connection --------------------
dbDisconnect(db)