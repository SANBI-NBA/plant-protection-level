# Collecting iNaturalist observations for all species in the sample that are not well-protected.
# First all relevant species are extracted from the protection level assessment database (PLAD)
# Then an API call is passed to iNaturalist to collect the data
# The data is processed to match the geospeciesoccurrences table in the PLAD
# And finally passed through to the database using an R-postgres connection

# ---- load libraries ---------------------------------
library(dplyr)     # For data wrangling
library(purrr)     # For processing json into a dataframe
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data
library(httr2)     # For API HTTP Requests
library(jsonlite)  # JSON ↔ R

# ---- API token ----------------------------

# Each time this script is run, you need to fetch a new authentication token from https://www.inaturalist.org/users/api_token
# Tokens expire after 24 hours (see https://www.inaturalist.org/pages/api+recommended+practices)
# Tokens are stored in the .Renviron file, and called from here
# After updating the API token, R needs to be restarted for changes to take effect
# So do this first before starting the rest of the script

api_token <- Sys.getenv("INAT_API_TOKEN")

# ---- database connection ------------------------------

# Note that this assumes a script called db-connection.R is present in the same folder as this script
# If the database is in a Docker container, it assumes that the container is running
# The database login information is contained in an .Renviron file

source("db-connection.R")

# ---- set up reusable variables for current assessment ---------

# These variables ensures that scripts are adjusted to the current assessment, and that data is attributed to the correct assessment

var_last_assessment_id <- 1 # For extracting Protection Level status of species from last assessment
var_current_assessment_id <- 2 # To ensure occurrence records are applied to current assessment
var_current_assessment_year <- 2025 
var_date_last_assessment <- "2007-01-01" # We only fetch new/recent inat records (to keep the API data request load down)

# ---- get species list for API call -------------------

speciesquery <- paste0("SELECT s.species_id, s.inat_id,  s.current_name, s.new_id, a.protectioncategory FROM speciesdata.tblspecies s
                  INNER JOIN speciesdata.tblplassessment a ON s.species_id = a.species_id WHERE a.assessment_id = ", var_last_assessment_id, 
                  " AND a.protectioncategory <> 'Well Protected' AND s.inat_id IS NOT NULL")

specieslist <- dbGetQuery(db, speciesquery)

taxonomy <- dbGetQuery(db, "SELECT species_id, inat_id, taxon FROM speciesdata.tblspecies")
taxonomy <- taxonomy %>% rename(new_inat_id = inat_id)

specieslist <- left_join(specieslist, taxonomy, by = c('new_id' = 'species_id'))
specieslist <- specieslist %>% mutate(inat_id = if_else(current_name == 0, new_inat_id, inat_id),
                                      species_id = if_else(current_name == 0, new_id, species_id))

# ---- API function ------------------------

fetch_inat_observations <- function(api_token,
                                    taxon_id,
                                    place_id = 6986,
                                    updated_since = var_date_last_assessment,
                                    per_page = 200,
                                    rate_limit_sec = 1) {
  # Base request with permanent query parameters ---------------------
  req <- request("https://api.inaturalist.org/v1/observations") |>
    req_headers(Authorization = paste("Bearer", api_token)) |>
    req_user_agent("sanbi-plant-assessment-script/0.1")    # polite UA, adjust as you like
  
  common_qs <- list(
    captive      = "false",
    geo          = "true",
    identified   = "true",
    introduced   = "false",
    mappable     = "true",
    native       = "true",
    place_id     = place_id,
    quality_grade= "research",
    updated_since= updated_since,
    order        = "desc",
    order_by     = "created_at",
    per_page     = per_page,
    taxon_id     = taxon_id        # <-- dynamic parameter
  )
  
  # Pagination loop --------------------------------------------------
  page <- 1
  total_pages <- 1                       # will be updated after 1st call
  results <- list()
  
  while (page <= total_pages) {
    qs <- c(common_qs, page = page)
    
    resp <- req |>
      req_url_query(!!!qs) |>
      req_perform()
    
    payload <- resp_body_json(resp, simplifyVector = FALSE)
    
    results <- c(results, payload$results)
    
    # after first iteration we know the total number of pages ----------
    if (page == 1) {
      total_pages <- ceiling(payload$total_results / payload$per_page)
    }
    
    message(sprintf("Fetched page %d / %d", page, total_pages))
    
    page <- page + 1
    if (page <= total_pages) Sys.sleep(rate_limit_sec) # obey 1-req/sec
  }
  
  invisible(results)
}

# ---- collect iNaturalist observations using API function --------------

# create an empty data frame to store the collected data
inat_observations <- data.frame()

# loop through species iNat ids, collect observations

for (i in specieslist$inat_id){
        
  # send API call
  obs <- fetch_inat_observations(api_token, i)
  
  # process the returned data into a data frame (only if data is returned)
  if (length(obs)>0){
  
    obs_df <- map_dfr(obs, function(x) {
      tibble(
        observation_id              = x$id %||% NA_integer_,
        observed_on                 = x$observed_on %||% NA_character_,
        user_name                   = pluck(x, "user", "name", .default = NA_character_),
        place_guess                 = x$place_guess %||% NA_character_,
        latitude                    = pluck(x, "geojson", "coordinates", 2, .default = NA_real_),
        longitude                   = pluck(x, "geojson", "coordinates", 1, .default = NA_real_),
        public_positional_accuracy = x$public_positional_accuracy %||% NA_real_,
        obscured                    = x$obscured %||% NA,
        geoprivacy                  = x$geoprivacy %||% NA_character_,
        taxon_name                  = pluck(x, "taxon", "name", .default = NA_character_)
      )
    })
  
  # add the requested taxon id as a column to the dataset (because iNat makes its own decisions about taxonomy
  # and then the data does not join back correctly to the species_id field)
  obs_df$taxon_id <- i
  
  # add this species' data to the other species' data
  inat_observations <- rbind(inat_observations, obs_df)
  
  # save the output (in case of loop breaking down)
  write.csv(inat_observations, file = paste0("inaturalist-observations-", var_current_assessment_year, ".csv"), row.names = FALSE)
  
  } else {
    message("No observations found. Moving on to next species.")
  }  

}

# ---- process data and insert into pl assessment database ----------------------

dat <- inat_observations %>% mutate(precisionm = as.integer(case_when(public_positional_accuracy <=100 ~ 100,
                                                                      public_positional_accuracy >100 ~ public_positional_accuracy,
                                                                      TRUE ~ 1000)),
                                    collection_year = as.integer(lubridate::year(observed_on)),
                                    locality_source = "iNaturalist API call",
                                    assessment_id = as.integer(var_current_assessment_id),
                                    qc = as.integer(0))

# join species ids 
dat <- left_join(dat, taxonomy, by=c('taxon_id' = 'new_inat_id'))

# select correct columns and rename

dat <- dat %>% select(assessment_id, species_id, latitude, longitude, precisionm, place_guess, user_name, 
               collection_year, locality_source, qc) %>% 
               rename(locality_description = place_guess,
                     collector = user_name)

# pass data to staging table in database
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpinatoccurrences"), value = dat, overwrite = TRUE)

# check that data is fine, especially that they all have lat/long values, and that there are no "NULL" species ids
# then insert the data into geospeciesoccurrences using this query

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
	  geom,
	  assessments)
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
	  ST_SetSRID(ST_MakePoint(longitude, latitude), 4326),
	  array[]::integer[]
from speciesdata.tmpinatoccurrences
")

# Delete the temporary table
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpinatoccurrences")

# ---- close database connection --------------------
dbDisconnect(db)



