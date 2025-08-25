# Script for calculating protected areas' contributions to species conservation targets
# This code assumes all data preparation has been done, and species targets have been set
# This script outputs a table of species x pa combinations

# ---- load libraries ---------------------------------
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data
library(terra)     # For doing spatial calculations
library(dplyr)     # For general data wrangling
library(gpkg)      # For reading spatial data in gpkg format

# ---- establish connection to input data folders on SANBI NBA data repository --------------------

# If this is the first time the script is run, locate the following folders in the plant_pl_assessment
# folder in the SANBI NBA data repository
#  - protected areas: protected-areas
#
# Add the paths to these folders to the following variables in your .Renviron file:
#  - protected areas: PA_DATA_PATH
# Restart R for changes to take effect

pa_file_path <- Sys.getenv("PA_DATA_PATH")

# ---- establish database connection ------------------

# Note that this assumes a script called db-connection.R is present in the same folder as this script
# If the database is in a Docker container, it assumes that the container is running
# The database login information is contained in an .Renviron file (not part of the repo)

source("db-connection.R")

# ---- set up reusable variables for current assessment ---------

# This means that the script can be reused for future assessments without needing to change anything other than
# these variables.

var_current_assessment_id <- 2

# ---- define custom crs ----------------------------------------

# So that data matches older suitable habitat models
# Note that this does not quite match the recommended NBA projection

pl_projection <- "+proj=aea +lat_0=0 +lon_0=24 +lat_1=-24 +lat_2=-32 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"

# ---- get information about time points and input files for the assessment from the PLAD ------

query_time_points <- paste0("SELECT * FROM speciesdata.refassessmenttimepoints WHERE assessment_id = ", var_current_assessment_id)
assessment_time_points <- dbGetQuery(db, query_time_points)

# ---- get list of species to be assessed from PLAD --------------

query_species <- paste0("SELECT s.species_id, s.map_id, s.taxon, s.resilient, t.number_occurrences,
                            t.population_status, t.population_target, t.area_target, t.subpopulation_target
                         FROM speciesdata.tblspecies s INNER JOIN speciesdata.tbltargets t ON
                            s.species_id = t.species_id 
                         WHERE s.current_name = 1 and t.assessment_id = ", var_current_assessment_id)

species_list <- dbGetQuery(db, query_species)

locality_targets_list <- species_list %>% tidyr::drop_na(subpopulation_target)

# ---- get occurrence data from PLAD --------------

query_occurrences <- paste0("SELECT occurrence_id, species_id, latitude, longitude, precisionm, min_count, max_count
                            FROM speciesdata.geospeciesoccurrences WHERE qc = 1 AND ",
                            var_current_assessment_id, " = ANY(assessments)")

occurrence_records <- dbGetQuery(db, query_occurrences)

# Filter occurrence records for species with locality targets
occurrence_records <- occurrence_records %>% filter(species_id %in% locality_targets_list$species_id)

# This imports the data as a dataframe (non-spatial)
# Then transform into SpatVector and project to match suitable habitat maps
occurrence_records <- terra::vect(occurrence_records, geom = c("longitude", "latitude"), crs = "EPSG:4326")
occurrence_records <- terra::project(occurrence_records, pl_projection)


# ---- ANALYSIS STEP 1: Define localities for species -------------------------

# Occurrence records >2km apart are considered separate localities
# Buffer localities by 1000m 
occurrence_buffers <- terra::buffer(occurrence_records, width=1000)
occurrence_dissolve <- terra::aggregate(occurrence_buffers, by = "species_id", dissolve = TRUE)
localities <- terra::disagg(occurrence_dissolve)

# ---- ANALYSIS STEP 2: For each time point intersect localities with relevant PA data -------------------------

# Set up and empty dataframe to collect results for each loop

species_pa_localities_time_points <- data.frame()

for(t in assessment_time_points$timepoint_id){
          
          current_time_point <- assessment_time_points %>% filter(timepoint_id == t)
  
          # load PA data 
          
          pa_layer_name <- current_time_point$pa_layer
          pa_file <- current_time_point$pa_map
          pa_file_path_current_time_point <- file.path(pa_file_path, pa_file)
          
          pa_conn <- gpkg_read(pa_file_path_current_time_point, connect = TRUE)
          pa <- gpkg_vect(pa_conn, pa_layer_name)
          pa <- project(pa, pl_projection)

          # Spatial intersect with PAs 

          species_pa_localities <- terra::intersect(localities, pa)
          species_pa_localities <- as.data.frame(species_pa_localities)
          species_pa_localities <- species_pa_localities %>% tidyr::drop_na(PA_ID) %>% 
                                group_by(species_id, PA_ID, PA_Sec_ID, Cluster_ID) %>% 
                                summarise(localities_pa = n()) %>% ungroup()
          
          # Add data indicating assessment and time point
          species_pa_localities <- species_pa_localities %>% mutate(assessment_id = var_current_assessment_id,
                                                                    timepoint_id = t)
          
          # Add results to collection dataframe
          species_pa_localities_time_points <- rbind(species_pa_localities_time_points, species_pa_localities)
}

# ---- ANALYSIS STEP 3: Process results and pass to db ----------------

# rename data columns because Postgres doesn't like capitals
# fix data types to be integers
species_pa_localities <- species_pa_localities_time_points %>% 
  rename(pa_id = PA_ID, pa_section_id = PA_Sec_ID, pa_cluster_id = Cluster_ID) %>% 
  # also fix all the data types
  mutate(pa_id = as.integer(pa_id),
         pa_section_id = as.integer(pa_section_id),
         pa_cluster_id = as.integer(pa_cluster_id))

# pass data to database
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpspecies_pa_localities"), 
             value = species_pa_localities, overwrite = TRUE)

# add data to tblspeciesinpa

query_insert_pa_localities <- "INSERT INTO speciesdata.tblspeciesinpa(
                                  assessment_id, species_id, pa_id, pa_section_id, pa_cluster_id, 
                                    presence_pa, localities_pa, timepoint_id)
                                SELECT assessment_id, species_id, pa_id, pa_section_id, pa_cluster_id, 
                                    1, localities_pa, timepoint_id
                                FROM speciesdata.tmpspecies_pa_localities"

dbExecute(db, query_insert_pa_localities)

# Remove temporary table
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpspecies_pa_localities")

# ---- close database connection --------------------
dbDisconnect(db)

