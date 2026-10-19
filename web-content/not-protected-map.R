# ---- load libraries ---------------------------------
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data
library(terra)     # For doing spatial calculations
library(dplyr)     # For general data wrangling


# ---- establish connection to input data folder on SANBI NBA data repository --------------------
map_file_path <- Sys.getenv("MAP_PATH")
reference_map_path <- Sys.getenv("MAP_INPUT_DATA_PATH")

# ---- establish database connection --------------------------------------------------------------
source("../workflow/db-connection.R")

# ---- set up reusable variables for current assessment ---------

# This means that the script can be reused for future assessments without needing to change anything other than
# these variables.

var_current_assessment_id <- 2
var_time_point_id <- 3

# --- get map ids of Not protected species ----------

qry_map_list <- paste0("select s.map_id
                            from speciesdata.tblspecies s inner join
                            speciesdata.tblplassessment a on
                            s.species_id = a.species_id
                            where s.map_id IS NOT NULL and
                            a.protectioncategory = 'Not Protected'::speciesdata.protectionlevelcategories and
                            a.timepoint_id = ", var_time_point_id)

map_list <- dbGetQuery(db, qry_map_list)

# --- create a reference raster map ------
# To ensure that all the species maps can be extended to the same extent

reference_map <- terra::rast(file.path(reference_map_path, "DEM90_Albers.tif"))
reference_map <- terra::subst(reference_map, 0:5000, 0, others=0)

for(i in map_list$map_id){
      map_file <- paste0(i, ".tif")
      species_map <- terra::rast(file.path(map_file_path, map_file))
      species_map <- terra::extend(species_map, ext(reference_map), snap="in")
      species_map <- terra::subst(species_map, 1, 1, others=0)
      species_map <- terra::resample(species_map, reference_map, method="modal")
      reference_map <- reference_map + species_map
}

writeRaster(reference_map, "output/plants-not-protected-map.tif", filetype = "GTiff", overwrite = TRUE)