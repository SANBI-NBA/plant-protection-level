# Script for calculating protected areas' contributions to species conservation targets
# This code assumes all data preparation has been done, and species targets have been set
# This script outputs a table of species x pa combinations

# ---- load libraries ---------------------------------
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data
library(terra)     # For doing spatial calculations
library(dplyr)     # For general data wrangling
library(gpkg)      # For reading spatial data in gpkg format
library(tidyterra) # For wrangling spatial data attributes


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

population_targets_list <- species_list %>% tidyr::drop_na(population_target)

# ---- get occurrence data from PLAD --------------

query_occurrences <- paste0("SELECT occurrence_id, species_id, latitude, longitude, precisionm, min_count, max_count
                            FROM speciesdata.geospeciesoccurrences WHERE qc = 1 AND ",
                            var_current_assessment_id, " = ANY(assessments)")

occurrence_records <- dbGetQuery(db, query_occurrences)

# Filter occurrence records for species with locality targets
occurrence_records <- occurrence_records %>% filter(species_id %in% population_targets_list$species_id)

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
localities <- localities %>% tidyterra::select(species_id)
localities$locality_id <- seq_len(nrow(localities))

# ---- ANALYSIS STEP 2: Estimate # individuals per locality ---------------------

# Find occurrence records with counts
occurrence_records_counts <- terra::subset(occurrence_records, 
                              !is.na(occurrence_records$min_count) | !is.na(occurrence_records$max_count))

# Add the row number as a value (this will help with joining the data back together again later)
occurrence_records_counts$count_id <- seq_len(nrow(occurrence_records_counts))

# Where counts are within the same uncertainty radius, it means that they could be
# counts by different observers of the same plants, or they could be counts of the same locality
# at different times. We want to aggregate these counts into one value, to avoid double counting
# such observations

occurrence_counts_buffers <- terra::buffer(occurrence_records_counts, width=occurrence_records_counts$precisionm)
occurrence_counts_buffers <- terra::aggregate(occurrence_counts_buffers, by = "species_id", dissolve = TRUE)
occurrence_counts_buffers <- terra::disagg(occurrence_counts_buffers)
occurrence_counts_buffers <- occurrence_counts_buffers %>% tidyterra::select(species_id)

# Add the row number as a value (this will help with joining the data back together again later)
occurrence_counts_buffers$count_buffer_id <- seq_len(nrow(occurrence_counts_buffers))

# Now we want to intersect occurrence_counts_buffers with occurrence_records counts
# To calculate an average number of plants in each overlapping count locality

occurrence_records_with_counts_by_buffer <- terra::intersect(occurrence_records_counts, occurrence_counts_buffers)

# We need to do some wrangling on the attributes, the geometry is not necessary for now
# Data summarised by count_buffer_id will be joined back to occurrence_counts_buffers_sf

occurrence_records_with_counts_by_buffer <- as.data.frame(occurrence_records_with_counts_by_buffer)
occurrence_counts_buffers_summary <- occurrence_records_with_counts_by_buffer %>% 
                                      filter(species_id_1 == species_id_2) %>% 
                                      group_by(species_id_1, count_buffer_id) %>% 
                                      summarise(min_count = round(mean(min_count, na.rm = TRUE)),
                                                max_count = round(mean(max_count, na.rm = TRUE))) %>% ungroup()

occurrence_counts_buffers_summary <- occurrence_counts_buffers_summary %>% 
                                      mutate(buffer_count = as.integer(case_when(is.nan(min_count) ~ max_count,
                                                                                 is.nan(max_count) ~ min_count,
                                                                                 max_count >= min_count ~ max_count,
                                                                                 min_count > max_count ~ min_count,
                                                                                 NA ~ TRUE))) %>% 
                                      select(species_id_1, count_buffer_id, buffer_count) %>% 
                                      rename(species_id = species_id_1)

# Join the wrangled data to occurrence_counts_buffers_sf
occurrence_counts_buffers <- dplyr::inner_join(occurrence_counts_buffers, 
                                                  occurrence_counts_buffers_summary, by = c("species_id", "count_buffer_id"))
  
                                      
# This now gets intersected with localities, where the buffer counts are summed to get a number of plants per locality
buffer_count_by_locality <- terra::intersect(localities, occurrence_counts_buffers) 

# We do similar data wrangling as for the buffers, but this time sum the counts per locality
buffer_count_by_locality <- as.data.frame(buffer_count_by_locality)

buffer_count_by_locality_summary <- buffer_count_by_locality %>% 
                                    filter(species_id_1 == species_id_2) %>% 
                                    group_by(species_id_1, locality_id) %>% 
                                    summarise(locality_count = as.integer(sum(buffer_count))) %>% ungroup() %>% 
                                    rename(species_id = species_id_1)

# We also create a refence df with average species counts per locality, to apply to localities without count data
species_mean_counts <- buffer_count_by_locality_summary %>% group_by(species_id) %>% 
                                    summarise(species_mean_count = as.integer(round(mean(locality_count)))) %>% ungroup()
# This should give the same number of records as in population_targets_list

# Join the processed count data to localities
localities <- dplyr::left_join(localities, species_mean_counts, by = "species_id")
localities <- dplyr::left_join(localities, buffer_count_by_locality_summary, by = c("species_id", "locality_id"))

# fill in NAs in locality counts with the mean counts for each species
localities <- localities %>% dplyr::mutate(locality_count = if_else(is.na(locality_count), 
                                                        species_mean_count, locality_count))

# ---- ANALYSIS STEP 3: For each time point intersect localities with relevant PA data ---------------------

# Create empty dataframe to collect results
population_in_pa <- data.frame()

for(t in assessment_time_points$timepoint_id){
          
      current_time_point <- assessment_time_points %>% filter(timepoint_id == t)
  
      # load PA data 
  
      pa_layer_name <- current_time_point$pa_layer
      pa_file <- current_time_point$pa_map
      pa_file_path_current_time_point <- file.path(pa_file_path, pa_file)
  
      pa_conn <- gpkg_read(pa_file_path_current_time_point, connect = TRUE)
      pa <- gpkg_vect(pa_conn, pa_layer_name)
      pa <- project(pa, pl_projection)
  
      # intersect
      localities_in_pa <- terra::intersect(localities, pa)

      # process results 
      localities_in_pa <- as.data.frame(localities_in_pa)

      population_in_pa_time_point <- localities_in_pa %>% tidyr::drop_na(PA_ID) %>% 
                                      group_by(species_id, PA_ID, PA_Sec_ID, Cluster_ID) %>% 
                                    summarise(localities_pa = as.integer(n()), population_pa = as.integer(sum(locality_count))) %>% ungroup()
      
      # Add assessment and time point data
      population_in_pa_time_point <- population_in_pa_time_point %>% mutate(assessment_id = var_current_assessment_id,
                                                                            timepoint_id = t)
      # Add to collection data frame
      population_in_pa <- rbind(population_in_pa, population_in_pa_time_point)
      
}

# ---- clean up data and pass to DB -------------------------

population_in_pa <- population_in_pa %>% 
                    rename(pa_id = PA_ID, pa_section_id = PA_Sec_ID, pa_cluster_id = Cluster_ID) %>% 
                    # also fix all the data types
                    mutate(pa_id = as.integer(pa_id),
                        pa_section_id = as.integer(pa_section_id),
                        pa_cluster_id = as.integer(pa_cluster_id))

dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmppopulation_in_pa"), 
             value = population_in_pa, overwrite = TRUE)

# add data to tblspeciesinpa

query_insert_population_in_pa <- "INSERT INTO speciesdata.tblspeciesinpa(
                                    assessment_id, species_id, pa_id, pa_section_id, pa_cluster_id, 
                                    presence_pa, localities_pa, population_pa, timepoint_id)
                                    SELECT assessment_id, species_id, pa_id, pa_section_id, pa_cluster_id, 
                                    1, localities_pa, population_pa, timepoint_id
                                    FROM speciesdata.tmppopulation_in_pa"

dbExecute(db, query_insert_population_in_pa)

# Remove temporary table
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmppopulation_in_pa")

# ---- close database connection --------------------
dbDisconnect(db)