# Use this script to produce a land cover layer aligned with suitable habitat models
# The output rasters of this process are used to calculate extent of available habitat for
# species inside protected areas at the time of assessment. It is necessary to use this approach
# because not all formal protected areas are 100% in natural condition - particularly World Heritage Sites
# and Protected Environments can have extensive areas converted to land uses such as agriculture or settlements
# and these can change over time. Therefore, for each protection level assessment the most suitable land cover 
# data for each time point in the assessment needs to be applied to the suitable habitat maps.

# ---- load libraries ---------------------------------
library(terra)     # For doing spatial calculations

# ---- establish connection to input data folders on SANBI NBA data repository --------------------

# If this is the first time the script is run, locate the following folders in the SANBI NBA data repository:
# - plant protection level suitable habitat map input data (as a reference raster)
# - national land cover data
# Add the paths to these folders to the following variables in your .Renviron file:
# - reference raster: MAP_INPUT_DATA_PATH
# - landcover: LC_DATA_PATH
# Restart R for changes to take effect

map_reference_file_path <- Sys.getenv("MAP_INPUT_DATA_PATH")
landcover_file_path <- Sys.getenv("LC_DATA_PATH")

# ---- load reference raster -----------------------------------------------

# Land cover data is resampled to one of the input layers of the suitable habitat maps, to make sure they align
# when raster data is intersected during the assessment

reference_raster <- terra::rast(file.path(map_reference_file_path, "DEM90_Albers.tif"))

# --- define land cover data series to be used in the assessment ---------------------

# Select the most relevant land cover layers from the SANBI land cover time series
# for each time point in the assessment, and add them to this list
land_cover_layers <- c(2014, 2018, 2022)

for(i in land_cover_layers){
      
  land_cover_file = paste0("nlc", i, "_7class.tif")
  landcover <- terra::rast(file.path(landcover_file_path, land_cover_file))
  
  # ---- reclassify and resample landcover -----------------------
  
  # Landcover is reclassified from the standard 7 classess to two classes: natural (1) and not natural (0)
  # However, species resilient to disturbance (weedy pioneer species) are likely to occur in secondary natural areas
  # while other native species typically do not. Therefore two reclassifications are done, one for weedy species 
  # where secondary natural = natural, and one for non-weedy species, where secondary natural = not natural
  
  reclass_natural <- terra::subst(landcover, 1, 1, others = 0)
  reclass_secondary <- terra::subst(landcover, 1:2, 1, others = 0)
  
  reclass_natural <- terra::project(reclass_natural, reference_raster, method = "near", mask = TRUE)
  reclass_secondary <- terra::project(reclass_secondary, reference_raster, method = "near", mask = TRUE)
  
  # ---- save outputs for use in spatial-analysis-species-x-pa.R ----------------------------------
  
  filename_natural <- paste0("lc-n-", i, ".tif")
  filename_secondary <- paste0("lc-s-", i, ".tif")
  
  # Write to .tif
  writeRaster(reclass_natural, file.path(map_reference_file_path, filename_natural), 
              filetype = "GTiff", overwrite = TRUE)
  writeRaster(reclass_secondary, file.path(map_reference_file_path, filename_secondary), 
              filetype = "GTiff", overwrite = TRUE)
  
  }

