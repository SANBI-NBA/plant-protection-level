# Generate suitable habitat maps for plant species
# This script reproduces earlier versions in QGIS model3 and Python
# It uses the same input data, ensuring that models generated with this script are comparable to those generated in QGIS
# There are many ways the process can be improved, but that would require regenerating all 900 maps
# Currently this script is set up to only produce maps for species needing new maps (as identified through processes
# in the prepare-map-data R script)

# ---- load libraries ---------------------------------
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data
library(terra)     # For doing spatial calculations
library(tidyterra) # For wrangling spatial data
library(dplyr)     # For general data wrangling

# ---- establish connection to input data folder on SANBI NBA data repository --------------------

# If this is the first time the script is run, locate the plant suitable habitat input data folder 
# in the SANBI NBA data repository. Add the path to this folder to a variable called 
# MAP_INPUT_DATA_PATH in your .Renviron file
# Restart R for changes to take effect

input_file_path <- Sys.getenv("MAP_INPUT_DATA_PATH")

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

# ---- extract species data from protection level database --------------------
# This code assumes that all the data checks in prepare map data has been done

# For point data, only QCd records are used. Make sure that point data for species needing maps have been checked
# and geospeciesoccurrences.qc has been set to 1 for records suitable for mapping

point_query <- paste0("select C.new_map_id, o.latitude, o.longitude, o.precisionm
    from speciesdata.geospeciesoccurrences o inner join speciesdata.tblspecieschanges c on
    o.species_id = c.new_species_id where c.new_map_id is not null and c.new_assessment_id = ", var_current_assessment_id, " 
    and o.qc = 1")

pointdata <- dbGetQuery(db, point_query)

dem_query <- paste0("select s.map_id, s.minalt, s.maxalt from speciesdata.tblspecies s
                      inner join speciesdata.tblspecieschanges c on s.species_id = c.new_species_id
                      where c.new_map_id is not null and c.new_assessment_id = ", var_current_assessment_id)

demdata <- dbGetQuery(db, dem_query)

landform_query <- paste0("select c.new_map_id, s.lf1, s.lf2, s.lf3, s.lf4, s.lf5, s.lf6
                        from speciesdata.tblspecieslandforms s inner join speciesdata.tblspecieschanges c
                        on s.species_id = c.new_species_id where c.new_map_id is not null and c.new_assessment_id = ",
                         var_current_assessment_id)

landformdata <- dbGetQuery(db, landform_query) #This should match the number of records in new_map_ids

veg_query <- paste0("select c.new_map_id, v.rastercode from speciesdata.tblspeciesvegetation s
                    inner join speciesdata.tblspecieschanges c on s.species_id = c.new_species_id
                    inner join speciesdata.refvegcodes v on s.mapcode = v.mapcode where c.new_map_id is not null
                    and c.new_assessment_id = ", var_current_assessment_id, " order by c.new_map_id, v.rastercode")

vegdata <- dbGetQuery(db, veg_query)

# ---- read suitable habitat variable rasters ----------------------------------------

# These layers are archived in the NBA Sharepoint Folder

dem <- terra::rast(file.path(input_file_path, "DEM90_Albers.tif"))
landforms <- terra::rast(file.path(input_file_path, "Landforms90_Albers.tif"))
vegmap <- terra::rast(file.path(input_file_path, "vegm2018.tif"))

# ---- wrangle point data --------------------

# First eliminate duplicate points (these cause trouble when checking whether a concave hull can be generated or not)
# Then transform into a spatial object with PL Albers projection

pointdata <- pointdata %>% mutate(latitude = round(latitude, 6),
                                  longitude = round(longitude, 6)) %>% 
                            group_by(new_map_id, latitude, longitude) %>% 
                            summarise(precisionm = min(precisionm)) %>% ungroup()

pointdata <- terra::vect(pointdata, geom = c("longitude", "latitude"), crs = "EPSG:4326")
pointdata <- terra::project(pointdata, pl_projection)

# ---- wrangle landform data from wide to long -------------------------------------------------

landformdata <- landformdata %>% tidyr::pivot_longer(cols = starts_with("lf"), names_to = "column", values_to = "value") %>%
                                  filter(value == 1) %>%
                                  mutate(landform = readr::parse_number(column)) %>%
                                  select(new_map_id, landform)

# ---- create maps -----------------------------------------------------------------------------

# Note that maps are saved to a directory named "suitable-habitat-models" within this R script's folder
# Make sure this directory exists before proceeding
# Make sure that map outputs are added to the suitable habitat map folder in the SANBI NBA data repository
# after completion of this script

# Make a dataframe to collect information about the process

output_summary <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(output_summary) <- c("map_id", "area_ha", "total_points", "points_in_habitat", "map_comment")


# Loop through the species list (in demdata) and make a suitable habitat map for each

for(i in demdata$map_id){

    # Select species points
    species_points <- pointdata %>% filter(new_map_id == i)
    
    # Check whether points > 2
    if(nrow(species_points)>2){
      # Then we can fit a concave hull
      species_hull <- terra::hull(species_points, type = "concave_ratio", param = 0.5)
      # Buffer the hull by 5 km
      species_buffer <- terra::buffer(species_hull, 5000)
    } else {
      species_buffer <- terra::buffer(species_points, species_points$precisionm)
    }
    
    # Crop landforms, DEM and vegmap to the buffer
    species_dem <- terra::crop(dem, species_buffer, mask = TRUE)
    species_landform <- terra::crop(landforms, species_buffer, mask = TRUE)
    species_veg <- terra::crop(vegmap, species_buffer, mask = TRUE)
    
    # Reclassify the cropped rasters to match species preferred habitat variables
    # Altitude
    species_dem_data <- demdata %>% filter(map_id == i) %>% select(minalt, maxalt)
    altrange <- with(species_dem_data, minalt:maxalt)
    species_dem_reclass <- terra::subst(species_dem, altrange, 1, others=0)
    # Landforms
    species_landform_data <- landformdata %>% filter(new_map_id == i) %>% select(landform) %>% mutate(value = 1)
    species_landform_data_from <- as.integer(species_landform_data$landform)
    species_landform_data_to <- as.integer(species_landform_data$value)
    species_landform_reclass <- terra::subst(species_landform, species_landform_data_from, species_landform_data_to, others=0)
    # Vegetation types
    species_veg_data <- vegdata %>% filter(new_map_id == i) %>% select(rastercode) %>% mutate(value = 1)
    species_veg_data_from <- as.integer(species_veg_data$rastercode)
    species_veg_data_to <- as.integer(species_veg_data$value)
    species_veg_reclass <- terra::subst(species_veg, species_veg_data_from, species_veg_data_to, others = 0)
    
    # Combine all three maps
    species_variables_combined <- species_dem_reclass + species_landform_reclass + species_veg_reclass
    species_variables_combined <- terra::subst(species_variables_combined, 3, 1, others = NA)
    
    # Save map
    filename <- paste0(i, ".tif")
    filepath <- file.path("suitable-habitat-models", filename)
    writeRaster(species_variables_combined, filepath, filetype = "GTiff", overwrite = TRUE)
    
    # Compute some values needed for target setting
    count_pixels <- freq(species_variables_combined)
    pixelsize <- res(species_variables_combined)
    area_ha <- round(count_pixels$count*pixelsize[1]*pixelsize[2]*0.0001, 0)
    
    # Raster output sense checks
    species_points_buffer <- terra::buffer(species_points, species_points$precisionm)
    pixels_per_point <- terra::zonal(species_variables_combined, species_points_buffer, "sum", na.rm=TRUE)
    total_points <- nrow(species_points)
    points_in_habitat <- nrow(tidyr::drop_na(pixels_per_point))
    map_comment <- if_else(points_in_habitat/total_points < 0.5, "Map underestimates habitat", "Map ok")
    
    # Collect information
    species_map_summary <- data.frame(map_id = i, area_ha = as.integer(area_ha), total_points = as.integer(total_points), points_in_habitat = as.integer(points_in_habitat), map_comment)
    output_summary <- rbind(output_summary, species_map_summary)
    write.csv(output_summary, file = "suitable-habitat-model-output-summary.csv", row.names = FALSE)
    
}

# ---- close database connection --------------------
dbDisconnect(db)