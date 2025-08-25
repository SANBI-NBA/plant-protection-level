# Script for checking and excluding out of range occurrence records
# This process must be executed before Protection Level Assessment can begin
# This script assumes that a .tif suitable habitat map exists for each species
# in the sampled species list.
# To read the .tif files, locate the habitat-models/maps folder in the SANBI NBA data repository
# Add the path to this folder to a variable called MAP_PATH in your .Renviron file before starting the script
# R will need to be restarted for changes in the .Renviron file to take effect

# ---- load libraries ---------------------------------
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data
library(terra)     # For doing spatial calculations
library(tidyterra) # For wrangling spatial data
library(dplyr)     # For general data wrangling
library(gpkg)      # For reading spatial data from gpkg files

# ---- establish connection to input data folder on SANBI NBA data repository --------------------
map_file_path <- Sys.getenv("MAP_PATH")

# ---- establish database connection --------------------------------------------------------------

# Note that this assumes a script called db-connection.R is present in the same folder as this script
# If the database is in a Docker container, it assumes that the container is running
# The database login information is contained in an .Renviron file (not part of the repo)

source("db-connection.R")

# ---- set up reusable variables for current assessment -----------------------------------------------

# This means that the script can be reused for future assessments without needing to change anything other than
# these variables.

var_current_assessment_id <- 2

# ---- define custom crs ------------------------------------------------------------------------------

# The projection used to generate suitable habitat maps
# Note that this does not quite match the recommended NBA projection

pl_projection <- "+proj=aea +lat_0=0 +lon_0=24 +lat_1=-24 +lat_2=-32 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"


# ---- get species list from database -----------------------------------------------------------------

# This query should return exactly 900 records (= number of species in SRLI)
species_list <- dbGetQuery(db, "SELECT species_id, map_id, taxon
                                FROM speciesdata.tblspecies WHERE current_name = 1")

# However, there is at least one species (Trichodiadema decorum) that is so poorly known, a map cannot be made for it
# Therefore, drop species where map_id is null

species_list <- species_list %>% tidyr::drop_na(map_id)

# ---- get occurrence records from database -----------------------------------------------------------

occurrence_records <- dbGetQuery(db, "SELECT occurrence_id, assessment_id, species_id,
                                 latitude, longitude, qc FROM speciesdata.geospeciesoccurrences")

# This imports the data as a dataframe (non-spatial)
# Then transform into SpatVector and project to match suitable habitat maps

occurrence_records <- terra::vect(occurrence_records, geom = c("longitude", "latitude"), crs = "EPSG:4326")
occurrence_records <- terra::project(occurrence_records, pl_projection)

# ---- Check that points overlap with range ----------------------------------------------------------

# Suitable habitat maps are modelled at a very fine scale, and occurrence records also have some uncertainty
# associated with them. This means even though points are accurate for the species, they may not directly intersect
# with suitable habitat. Instead, a point is included in the assessment when it is somewhere within the general range
# of the species. Therefore suitable habitats need to be generalized to a range map before they can be used as a basis
# for QCing points

# Create an empty data frame to collect QC outputs
occurrences_qc <- data.frame()

# Loop through species occurrence records and check against maps

for(i in species_list$species_id){
    
    # Extract species occurrence records
    species_occurrences <- occurrence_records %>% filter(species_id == i)

    # Locate and import species map
    map_id <- species_list$map_id[species_list$species_id == i]
    map_file <- paste0(map_id, ".tif")
    species_map <- terra::rast(file.path(map_file_path, map_file))
    
    # Generalize SH map to range map using buffer
    # First extent of raster map must be extended to accommodate areas outside the current raster extent
    # Use the extent of the occurrence records (which may or may not be in areas outside the current raster)
    range_map <- terra::extend(species_map, ext(species_occurrences))
    range_map <- terra::buffer(range_map, 10000, background = 0)
    
    # Extract raster values for each point (which will indicate whether it is in or outside the buffer)
    qc <- extract(range_map, species_occurrences)
    species_occurrences_qc <- cbind(species_occurrences, qc)
    species_occurrences_qc <- as.data.frame(species_occurrences_qc)
    species_occurrences_qc <- species_occurrences_qc %>% mutate(qc = if_else(.[[6]] == TRUE, 1, 0)) %>% 
                              select(occurrence_id, assessment_id, species_id, qc)
    
    # Combine output with other species' results
    occurrences_qc <- rbind(occurrences_qc, species_occurrences_qc)
    # Export results to csv in case process is interrupted
    write.csv(occurrences_qc, file = "occurrences-qc.csv", row.names = FALSE)
}   
    
# ---- Update QC in protection level database --------------------------------------------------------

occurrences_qc <- occurrences_qc %>% mutate(qc = as.integer(qc))

dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpqc"), value = occurrences_qc, overwrite = TRUE)
    
# Update data in geospeciesoccurrences
dbExecute(db, "UPDATE speciesdata.geospeciesoccurrences as o
                SET qc = q.qc
                FROM speciesdata.tmpqc as q
                WHERE o.occurrence_id = q.occurrence_id") 

# Assign QCd occurrence records to the current assessment
query_qc <- paste0("UPDATE speciesdata.geospeciesoccurrences SET assessments = assessments || ", var_current_assessment_id,
                  " WHERE qc = 1 AND assessment_id = ", var_current_assessment_id,
                  " AND NOT (", var_current_assessment_id, " = ANY(assessments))")

dbExecute(db, query_qc)   

# clean up the temporary table
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpqc")

# ---- close database connection --------------------
dbDisconnect(db)

    
    