# Processing species data exported from the plant Red List database
# Data was provided as a combination of Red List data (status, population size, etc)
# and occurrence records. This script separates the RL data from the occurrence records
# and passes it to the PL database in Docker

# ---- load libraries ---------------------------------
library(dplyr)     # For data wrangling
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data

# ---- set up reusable variables for current assessment ---------

# These variables ensures that scripts are adjusted to the current assessment, and that data is attributed to the correct assessment

var_last_assessment_id <- 1 # For extracting comparative data from last assessment
var_current_assessment_id <- 2 # To ensure new occurrence records are applied to current assessment
var_current_assessment_year <- 2025 


# ---- load Red List data ----------------------------
# This data was extracted via a query on the MS Access plant Red List database on 30 June 2025
# The data snapshot is archived in the plant_pl_assessment folder on the SANBI NBA OneDrive

rl_data <- read.csv("rl-occurrence-records-2025.csv", header = TRUE)

# ---- split data into occurrence records and other Red List data -----------------------------------

occurrence_data <- rl_data %>% select(Genspec, DDS, DDE, Precision, Description, Collector, Coll_Year,
                                      Source, Min.No_indivs, Max.No_indivs)

assessment_data <- rl_data %>% group_by(Genspec, Taxon, NATIONAL.STATUS, National.Criteria, National.Assessment.Date,
                                        National.min.pop.size, National.max.pop.size, Population.Description) %>% 
                                summarise(count_occurrences = n()) %>% ungroup()

# Check that assessment_data contains 900 records (i.e. all species in RLI sample are represented)

# save the assessment data as snapshot for archiving in SANBI NBA OneDrive
write.csv(assessment_data, file = paste0("rl-data-", var_current_assessment_year, ".csv"), row.names = FALSE)

# ---- database connection ------------------------------

# Note that this assumes a script called db-connection.R is present in the same folder as this script
# If the database is in a Docker container, it assumes that the container is running
# The database login information is contained in an .Renviron file (not part of the repo)

source("db-connection.R")

# Pull out species list
# This assumes the taxonomy in the Red List data has been already updated in the PL assessment species list
taxonomy <- dbGetQuery(db, "SELECT species_id, rl_id, taxon FROM speciesdata.tblspecies")

# Pull out occurrence records from previous assessments
# These records are matched as far as possible with new data to avoid data duplication, only relevant fields are selected

occurrence_query <- paste0("SELECT occurrence_id, species_id, latitude, longitude, locality_description,
                            collector, collection_year, locality_source FROM speciesdata.geospeciesoccurrences
                              WHERE NOT(", var_current_assessment_id, "= ANY(assessments))")

reference_occurrences <- dbGetQuery(db, occurrence_query)

# Pull out previous assessment's RL data for comparison

rl_query <- paste0("select a.assessment_id, s.species_id, s.taxon, s.rl_status, s.rl_criteria, t.min_population_size,
  t.max_population_size, s.map_id, s.current_name, s.new_id from speciesdata.tblplassessment a
inner join speciesdata.tblspecies s on a.species_id = s.species_id
inner join speciesdata.tbltargets t on s.species_id = t.species_id
where a.assessment_id = ", var_last_assessment_id)

previous_rl_data <- dbGetQuery(db, rl_query)

# ---- process new occurrence data ---------------------------

occurrence_data <- occurrence_data %>% mutate(precisionm = as.integer(case_when(Precision == "" ~ 5000,
                                                                                Precision %in% c("10", "50") ~ 100,
                                                                                Precision == "100" ~ 100,
                                                                                Precision == "1000" ~ 1000,
                                                                                Precision == "10000" ~ 10000,
                                                                                Precision == "100000" ~ 100000,
                                                                                Precision == "2000" ~ 2000,
                                                                                Precision == "250" ~ 250,
                                                                                Precision == "500" ~ 500,
                                                                                Precision %in% c("5000", "farm", "unknown") ~ 5000,
                                                                                Precision == "50000" ~ 50000,
                                                                                Precision == "50000" ~ 50000,
                                                                                Precision == "QDS" ~ 15000,
                                                                                Precision == "reserve" ~ 10000,
                                                                                TRUE ~ 5000)),
                                              Coll_Year = as.integer(if_else(is.na(Coll_Year) | Coll_Year %in% 0, 9999, Coll_Year)),
                                              Collector = if_else(is.na(Collector), "", Collector),
                                              Description = if_else(is.na(Description), "", Description),
                                              Source = if_else(is.na(Source), "Unknown", Source))

occurrence_data <- occurrence_data %>% select(Genspec, DDS, DDE, precisionm, Description, Collector, Coll_Year, Source,
                                              Min.No_indivs, Max.No_indivs) %>% 
                    rename(latitude = DDS, longitude = DDE, locality_description = Description,
                           collector = Collector, collection_year = Coll_Year,
                           locality_source = Source, min_count = Min.No_indivs, max_count = Max.No_indivs)

occurrence_data <- left_join(occurrence_data, taxonomy, by = c("Genspec" = "rl_id"))


# Check for species that did not join
occurrence_missing_species_ids <- occurrence_data %>% filter(is.na(species_id)) %>% distinct(Genspec)
occurrence_missing_species_ids <- left_join(occurrence_missing_species_ids, assessment_data, by = "Genspec")

# Fix them if possible.
# For the 2025 assessment:

occurrence_data <- occurrence_data %>% mutate(species_id = as.integer(case_when(Genspec == "2489-61" ~ 412,
                                                                     Genspec == "2039-4005" ~ 109,
                                                                     TRUE ~ species_id)))

# ---- prepare reference occurrence data for joining ---------------------------

# Treat NAs in joining fields in the same way as the new occurrence records to make sure they match
reference_occurrences <- reference_occurrences %>% mutate(collection_year = if_else(is.na(collection_year) | collection_year %in% 0, 9999, collection_year),
                                                          collector = if_else(is.na(collector), "", collector),
                                                          locality_description = if_else(is.na(locality_description), "", locality_description),
                                                          locality_source = if_else(is.na(locality_source), "Unknown", locality_source))

# ---- split new occurrence data into matching and non-matching data ---------------------------

matching_occurrence_data <- inner_join(occurrence_data, reference_occurrences, by = c("species_id", "latitude", 
                                                                                      "longitude", "locality_description",
                                                                                      "collector", "collection_year", "locality_source"))

new_occurrence_data <- anti_join(occurrence_data, reference_occurrences, by = c("species_id", "latitude", 
                                                                                      "longitude", "locality_description",
                                                                                      "collector", "collection_year", "locality_source"))

# Fix placeholder collection_year on new data
new_occurrence_data <- new_occurrence_data %>% mutate(collection_year = as.integer(if_else(collection_year == 9999, NA, collection_year)))

# Check and remove records without latitude and longitude
new_occurrence_data <- new_occurrence_data %>% tidyr::drop_na(any_of(c("latitude", "longitude")))

# ---- pass data to protection level database ---------------------------------------------------------------
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpnew_occurrence_data"), value = new_occurrence_data, overwrite = TRUE)
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpmatching_occurrence_data"), value = matching_occurrence_data, overwrite = TRUE)

# check that the data transferred ok - no NAs on geometry and foreign keys (species_id), check data types (especially integer vs numeric)
# then use the following statements to insert the data into geospeciesoccurrences

query_update <- paste0("UPDATE speciesdata.geospeciesoccurrences SET assessments = assessments || ",
                       var_current_assessment_id, " FROM (SELECT DISTINCT occurrence_id FROM speciesdata.tmpmatching_occurrence_data) b
                       WHERE speciesdata.geospeciesoccurrences.occurrence_id = b.occurrence_id")
dbExecute(db, query_update)   

# Delete the temporary table
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpmatching_occurrence_data")

query_insert <- paste0("insert into speciesdata.geospeciesoccurrences (assessment_id, species_id, latitude, longitude, precisionm, locality_description,
	  collector,  collection_year, locality_source, min_count, max_count, geom, assessments) select ",
    var_current_assessment_id,
    ", species_id, latitude, longitude, precisionm, locality_description, collector, collection_year, locality_source, min_count, max_count, 
	  ST_SetSRID(ST_MakePoint(longitude, latitude), 4326), array[]::integer[] from speciesdata.tmpnew_occurrence_data")

dbExecute(db, query_insert)

# Delete the temporary table
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpnew_occurrence_data")

# ---- process RL data --------------------------------------------------------------

# join taxonomy list to new RL data to get species_ids
new_rl_data <- left_join(assessment_data, taxonomy, by = c("Genspec" = "rl_id"))

# in the 2025 assessment there were two species not joining due to taxonomic mix ups
# they were dropped from the update list here and the previous assessment's data retained
new_rl_data <- new_rl_data %>% tidyr::drop_na(species_id)

# fix column names
new_rl_data <- new_rl_data %>% rename(rl_status = NATIONAL.STATUS, rl_criteria = National.Criteria,
                                      rl_date = National.Assessment.Date, min_population_size = National.min.pop.size,
                                      max_population_size = National.max.pop.size, population_description = Population.Description)

new_rl_data <- new_rl_data %>% select(species_id, taxon, rl_status, rl_criteria, rl_date, min_population_size,
                                  max_population_size, population_description)

# pass this data to a staging table in the postgres db
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpnewrldata"), value = new_rl_data, overwrite = TRUE)
# this data gets processed later (when setting new targets for current assessment)

# ---- detect and record changes in RL data from last assessment --------------------

new_rl_data <- new_rl_data %>% mutate(assessment_id = as.integer(var_current_assessment_id)) %>% 
                    select(assessment_id, species_id, taxon, rl_status, rl_criteria, min_population_size, max_population_size)

# rename columns to avoid R's automatic column names when joining
colnames(previous_rl_data) <- paste('previous', colnames(previous_rl_data), sep = '_')
colnames(new_rl_data) <- paste('new', colnames(new_rl_data), sep = '_')

# prepare joining field
previous_rl_data <- previous_rl_data %>% mutate(previous_new_id = if_else(previous_current_name == 1, previous_species_id, previous_new_id))

rl_data_comparison <- inner_join(previous_rl_data, new_rl_data, by = c("previous_new_id" = "new_species_id"))

# we have to replace the NAs with data to enable previous and new data comparisons
rl_data_comparison <- rl_data_comparison %>% mutate(previous_rl_criteria = if_else(is.na(previous_rl_criteria), "", previous_rl_criteria),
                                                    new_rl_criteria = if_else(is.na(new_rl_criteria), "", new_rl_criteria),
                                                    previous_min_population_size = as.integer(if_else(is.na(previous_min_population_size), -1, previous_min_population_size)),
                                                    previous_max_population_size = as.integer(if_else(is.na(previous_max_population_size), -1, previous_max_population_size)),
                                                    new_min_population_size = as.integer(if_else(is.na(new_min_population_size), -1, new_min_population_size)),
                                                    new_max_population_size = as.integer(if_else(is.na(new_max_population_size), -1, new_max_population_size))) %>% 
                      rename(new_species_id = previous_new_id)


# find records where taxon, rl status or population size does not match

rl_data_comparison <- rl_data_comparison %>% mutate(rl_change = as.integer(if_else(previous_rl_status != new_rl_status | previous_rl_criteria != new_rl_criteria, 1, 0)),
                                                    population_change = as.integer(if_else(previous_min_population_size != new_min_population_size | previous_max_population_size != new_max_population_size, 1, 0)),
                                                    taxonomic_revision = as.integer(if_else(previous_species_id != new_species_id, 1, 0)))

rl_data_comparison <- rl_data_comparison %>% filter(rl_change == 1 | population_change == 1 | taxonomic_revision == 1)

# clean out placehoder population sizes
rl_data_comparison <- rl_data_comparison %>% mutate(previous_min_population_size = if_else(previous_min_population_size == -1, NA, previous_min_population_size),
                                                    previous_max_population_size = if_else(previous_max_population_size == -1, NA, previous_max_population_size),
                                                    new_min_population_size = if_else(new_min_population_size == -1, NA, new_min_population_size),
                                                    new_max_population_size = if_else(new_max_population_size == -1, NA, new_max_population_size))



# pass this data to a staging table in the postgres db
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpspeciesdatachanges"), value = rl_data_comparison, overwrite = TRUE)

# insert the data into the correct table in the database (after checking that the data is ok)

dbExecute(db,
          "insert into speciesdata.tblspecieschanges(
previous_assessment_id,
	previous_species_id,
	previous_taxon,
	previous_rl_status,
	previous_rl_criteria,
	previous_min_population_size,
	previous_max_population_size,
	previous_map_id,
	new_assessment_id,
	new_species_id,
	new_taxon,
	new_rl_status,
	new_rl_criteria,
	new_min_population_size,
	new_max_population_size,
	rl_change,
	population_change,
	taxonomic_revision)
select
 previous_assessment_id,
	previous_species_id,
	previous_taxon,
	previous_rl_status,
	previous_rl_criteria,
	previous_min_population_size,
	previous_max_population_size,
	previous_map_id,
	new_assessment_id,
	new_species_id,
	new_taxon,
	new_rl_status,
	new_rl_criteria,
	new_min_population_size,
	new_max_population_size,
	rl_change,
	population_change,
	taxonomic_revision
from speciesdata.tmpspeciesdatachanges")

# Drop the temporary table
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpspeciesdatachanges")

# ---- close database connection --------------------
dbDisconnect(db)