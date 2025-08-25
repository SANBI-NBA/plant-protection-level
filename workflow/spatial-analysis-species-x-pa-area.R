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
#  - plant suitable habitat maps: habitat-models/maps 
#  - protected areas: protected-areas
#  - land cover: habitat-models/data
#
# Add the paths to these folders to the following variables in your .Renviron file:
#  - plant suitable habitat maps: MAP_PATH 
#  - protected areas: PA_DATA_PATH
#  - land cover: MAP_INPUT_DATA_PATH
# Restart R for changes to take effect

map_file_path <- Sys.getenv("MAP_PATH")
pa_file_path <- Sys.getenv("PA_DATA_PATH")
lc_file_path <- Sys.getenv("MAP_INPUT_DATA_PATH")

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

# ---- get occurrence data from PLAD --------------

query_occurrences <- paste0("SELECT occurrence_id, species_id, latitude, longitude, precisionm, min_count, max_count
                            FROM speciesdata.geospeciesoccurrences WHERE qc = 1 AND ",
                            var_current_assessment_id, " = ANY(assessments)")

occurrence_records <- dbGetQuery(db, query_occurrences)

# This imports the data as a dataframe (non-spatial)
# Then transform into SpatVector and project to match suitable habitat maps
occurrence_records <- terra::vect(occurrence_records, geom = c("longitude", "latitude"), crs = "EPSG:4326")
occurrence_records <- terra::project(occurrence_records, pl_projection)


# ---- ANALYSIS: Loop through time points and calculate area contributions of PAs for each species ----

# Set up some additional dfs to be used within loop

# extract from species_list only the species with area targets and that have maps available
area_targets_list <- species_list %>% tidyr::drop_na(map_id, area_target)

species_pa_presence_all_time_points <- data.frame()
area_targets_data_all_time_points <- data.frame()

# Set up a counter to track progress
total_species <- nrow(area_targets_list)

for(t in assessment_time_points$timepoint_id){
              current_time_point <- assessment_time_points %>% filter(timepoint_id == t)

        # ---- STEP 1: spatial join between species occurrences and PA polygons ------------------

        pa_layer_name <- current_time_point$pa_layer
        pa_file <- current_time_point$pa_map
        pa_file_path_current_time_point <- file.path(pa_file_path, pa_file)
        
        pa_conn <- gpkg_read(pa_file_path_current_time_point, connect = TRUE)
        pa <- gpkg_vect(pa_conn, pa_layer_name)
        pa <- project(pa, pl_projection)

        # Records which species are present in which PAs. This step is fundamental to confirming presences
        # of species in PAs regardless of which assessment method is used.

        species_pa_presence <- terra::intersect(occurrence_records, pa)
        species_pa_presence <- as.data.frame(species_pa_presence)
        species_pa_presence <- species_pa_presence %>% tidyr::drop_na(PA_ID) %>% 
                        group_by(species_id, PA_ID, PA_Sec_ID, Cluster_ID) %>% 
                        summarise(number_occurrences = n()) %>% ungroup()
        species_pa_presence <- species_pa_presence %>% mutate(assessment_id = var_current_assessment_id,
                                                              timepoint_id = t)
        
        species_pa_presence_all_time_points <- rbind(species_pa_presence_all_time_points, species_pa_presence)

        # ---- STEP 2: Intersect species maps with PAs -----------------------------

        # These are species where the target is set on suitable habitat
        # For each species, their suitable habitat map (SHM) is first intersected with the applicable landcover layer
        # to remove areas where habitat no longer exists. Then the SHM is intersected with the PA layer and the area of
        # habitat in each PA it intersects with is calculated. The output of step 1 is used to confirm presence of species
        # in intersected PAs

        # Load LC layers

        lc_n_file <- current_time_point$lc_map_natural
        lc_n_file_path <- file.path(lc_file_path, lc_n_file)
        lc_n <- terra::rast(lc_n_file_path)

        lc_s_file <- current_time_point$lc_map_secondary_natural
        lc_s_file_path <- file.path(lc_file_path, lc_s_file)
        lc_s <- terra::rast(lc_s_file_path)

        # create empty dataframe to collect results
        area_targets_data <- data.frame()

        # Loop through species maps and intersect with PAs
        species_counter <- 0
        
        for(i in area_targets_list$species_id){
        
              current_species <- area_targets_list %>% filter(species_id == i)
              
              # Load species map
              map_file <- paste0(current_species$map_id, ".tif")
              species_map <- terra::rast(file.path(map_file_path, map_file))
              
              # Erase transformed areas
              if(current_species$resilient == 1){
                local_lc <- terra::crop(lc_s, ext(species_map), mask = TRUE)
              } else {  
                local_lc <- terra::crop(lc_n, ext(species_map), mask = TRUE)
              }
              
              # For older maps the pixels do not align so we have to do a resample
              species_map <- terra::resample(species_map, local_lc, method = "near")
              species_map <- species_map + local_lc
              species_map <- terra::subst(species_map, 2, 1, others=0)
              
              # Calculate habitat inside PAs
              species_pa_habitat <- terra::zonal(species_map, pa, fun = "sum", na.rm = TRUE)
              species_pa_habitat <- cbind(species_pa_habitat, pa)
              species_pa_habitat <- subset(species_pa_habitat, !is.nan(species_pa_habitat[[1]])) #This is to take out NaN
              species_pa_habitat <- species_pa_habitat %>% filter(species_pa_habitat[[1]] != 0)
              if(nrow(species_pa_habitat) != 0){
                  pixelsize <- res(species_map)
                  species_pa_habitat <- species_pa_habitat %>% 
                      mutate(area_habitat_ha = round(species_pa_habitat[[1]]*pixelsize[1]*pixelsize[2]*0.0001, 0),
                             species_id = i) %>% 
                      select(species_id, PA_ID, PA_Sec_ID, Cluster_ID, area_habitat_ha)
              
                  # Add results to data frame
                  area_targets_data <- rbind(area_targets_data, species_pa_habitat)
                  
                  # Save outputs 
                  write.csv(area_targets_data, file = paste0("species-x-pa-area-", t, ".csv"), row.names = FALSE)
                  
                  species_counter <- species_counter +1
                  print (paste0("Species ", i, " done. Species ", 
                                species_counter, " of ", total_species, " (timepoint ", t, ")"))
                
              } else { 
                species_counter <- species_counter +1
                print (paste0("Species ", i, " not in any PAs. ",
                              species_counter, " of ", total_species, " (timepoint ", t, ")"))
              }
              
        }
        area_targets_data <- area_targets_data %>% mutate(assessment_id = var_current_assessment_id,
                                                          timepoint_id = t)
        area_targets_data_all_time_points <- rbind(area_targets_data_all_time_points, area_targets_data)
        # Save outputs 
        write.csv(area_targets_data_all_time_points, file ="species-x-pa-area-timepoints.csv", row.names = FALSE)

        print (paste0("Time point  ", t, " completed"))
}

# ---- ANALYSIS STEP 3: Process results and pass to db --------------------------

# rename data columns because Postgres doesn't like capitals
area_targets_data <- area_targets_data_all_time_points %>% 
                      rename(pa_id = PA_ID, pa_section_id = PA_Sec_ID, pa_cluster_id = Cluster_ID)

# also fix all the data types
area_targets_data <- area_targets_data %>% mutate(pa_id = as.integer(pa_id),
                                                  pa_section_id = as.integer(pa_section_id),
                                                  pa_cluster_id = as.integer(pa_cluster_id),
                                                  area_habitat_ha = as.integer(area_habitat_ha))

# do the same with species_pa_presence
species_pa_presence <- species_pa_presence_all_time_points %>% 
                        rename(pa_id = PA_ID, pa_section_id = PA_Sec_ID, pa_cluster_id = Cluster_ID) %>% 
                        mutate(pa_id = as.integer(pa_id),
                               pa_section_id = as.integer(pa_section_id),
                               pa_cluster_id = as.integer(pa_cluster_id))

# pass data to database
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmparea_targets_data"), 
             value = area_targets_data, overwrite = TRUE)
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpspecies_pa_presence"), 
             value = species_pa_presence, overwrite = TRUE)

# add data to tblspeciesinpa

query_insert_pa_area_data <- "INSERT INTO speciesdata.tblspeciesinpa(
                                    assessment_id, species_id, pa_id, pa_section_id, pa_cluster_id, area_habitat_pa, timepoint_id)
                                    SELECT assessment_id, species_id, pa_id, pa_section_id, 
                                        pa_cluster_id, area_habitat_ha, timepoint_id
                                    FROM speciesdata.tmparea_targets_data"

dbExecute(db, query_insert_pa_area_data)

query_set_presence_pa <- "UPDATE speciesdata.tblspeciesinpa AS sp
                                SET presence_pa = 1 
                                FROM speciesdata.tmpspecies_pa_presence AS pp
                                WHERE sp.species_id = pp.species_id AND 
                                sp.pa_id = pp.pa_id AND sp.pa_section_id = pp.pa_section_id AND sp.pa_cluster_id = pp.pa_cluster_id 
                                AND sp.assessment_id = pp.assessment_id AND sp.timepoint_id = pp.timepoint_id"

dbExecute(db, query_set_presence_pa)

query_set_presence_cluster <- "UPDATE speciesdata.tblspeciesinpa AS sp
                                SET presence_cluster = 1 
                                FROM speciesdata.tmpspecies_pa_presence AS pp
                                WHERE sp.species_id = pp.species_id AND 
                                sp.pa_cluster_id = pp.pa_cluster_id 
                                AND sp.assessment_id = pp.assessment_id AND
                                sp.timepoint_id = pp.timepoint_id"

dbExecute(db, query_set_presence_cluster)

# Remove temporary tables
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmparea_targets_data")
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpspecies_pa_presence")

# ---- close database connection --------------------
dbDisconnect(db)