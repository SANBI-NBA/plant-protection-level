# This script documents how PA effectiveness was determined for the 2025 PL assessment
# This is intended as a permanent record of the process for archiving purposes, rather than a reusable script
# The process combined different data sources available at the time:
#  - precentage cover of alien invasive plants (compiled for the country from various datasets) - already added to 
#        PA data at the point where this process started
#  - land cover change within PAs between 2014 and 2022 (standardised 7-class SA landcover)
#  - PA effectiveness scores and notes from 2018 PL assessment - these were derived mostly from METT
#        reports and threats associated with localities in the plant Red List database; a few were from
#        personal observations by LVS and others (Nick Helme, Ismail Ebrahim, etc.)
#  - PA effectiveness notes contributed by experts in 2025 

# ---- load libraries ---------------------------------
library(terra)     # For doing spatial calculations
library(tidyterra) # For attribute wrangling of SpatVectors
library(dplyr)     # For general data wrangling
library(gpkg)      # For reading spatial data in gpkg format
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data

# ---- establish connection to input data folders on SANBI NBA data repository --------------------

# These file paths are stored in the .Renviron file
# The following paths were used:
#  - protected areas: PA_DATA_PATH
#  - land cover: MAP_INPUT_DATA_PATH

pa_file_path <- Sys.getenv("PA_DATA_PATH")
lc_file_path <- Sys.getenv("MAP_INPUT_DATA_PATH")

# ---- define custom crs ----------------------------------------

# Note that this does not quite match the recommended NBA projection
# The PAs from the 2018 assessment and LC data is already in this projection
# The PA data created for the 2025 assessment (2024 and 2017) is in the NBA projection

pl_projection <- "+proj=aea +lat_0=0 +lon_0=24 +lat_1=-24 +lat_2=-32 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"


# ---- load PA data -------------------------------------

# This is the PA data that was used in the NBA 2018:
pa_2018_conn <- gpkg_read(file.path(pa_file_path, "pas-2018-assessment.gpkg"), connect = TRUE)
pa_2018 <- gpkg_vect(pa_2018_conn, "pas-2018")

# 2017 PA data extracted from 2024/2025 PA data
pa_2017_conn <- gpkg_read(file.path(pa_file_path, "pas-2017.gpkg"), connect = TRUE)
pa_2017 <- gpkg_vect(pa_2017_conn, "pas-2017")
pa_2017 <- project(pa_2017, pl_projection)

# 2025 assessment PA data (2024)
pa_2025_conn <- gpkg_read(file.path(pa_file_path, "pas-2025.gpkg"), connect = TRUE)
pa_2025 <- gpkg_vect(pa_2025_conn, "pas-2025")
pa_2025 <- project(pa_2025, pl_projection)

# ---- load LC data -------------------------------------

# Only using the LC data where secondary natural is considered not natural
# LC 2025 is actually 2022 (it is just named that way for the 2025 assessment)

lc_2014 <- terra::rast(file.path(lc_file_path, "lc-n-2014.tif"))
lc_2018 <- terra::rast(file.path(lc_file_path, "lc-n-2018.tif"))
lc_2025 <- terra::rast(file.path(lc_file_path, "lc-n-2025.tif"))

###########################################
#                                         #
#         SETTING EFFECTIVENESS           #
#                                         #
###########################################

# GENERAL NOTES:
# Effectiveness is set for each component separately, using a set of rules
#  - Alien invasive plants
#  - Land cover & land cover change
#  - Other sources (METT, experts)
# Whichever gives the worst score is taken as the general/overall effectiveness of the PA

# The same AIP data is used for both 2018 and 2025. AIPs probably got worse in some PAs over this time
# While in others there may have been clearing efforts (although these are rarely successful in the long term)
# In other words, data is not suitable to reliably detect changes in effectiveness due to changes in AIP coverage
# between 2018 and 2025, so unless other factors are present, PAs will have the same scores for 2018 and 2025

# ---- Alien invasive plants -----------------------

# AIP rules:
#  - >50% cover - effectiveness = poor
#  - Between 10 and 50% cover - effectiveness = fair
#  - <10% cover - effectiveness = good

pa_2017 <- pa_2017 %>% mutate(aip_effectiveness = case_when(Perc_Inv >= 50 ~ 1,
                                                            Perc_Inv >=10 & Perc_Inv < 50 ~ 2,
                                                            Perc_Inv <10 ~ 3,
                                                            TRUE ~ NA))

pa_2025 <- pa_2025 %>% mutate(aip_effectiveness = case_when(Perc_Inva >= 50 ~ 1,
                                                            Perc_Inva >=10 & Perc_Inva < 50 ~ 2,
                                                            Perc_Inva <10 ~ 3,
                                                            TRUE ~ NA))

# ---- Land cover and land cover change -------------------------

# Two things are considered here: overall percentage of PA in natural condition
# Loss of natural areas within PA between 2014 and 2018 for 2017 data,
# and between 2018 and 2022 for 2025 data.
# Land cover layers are already classified as 1 = natural, 0 = not natural
# Therefore a mean zonal statistic would give percentage natural if multiplied by 100

# 2017
pa_2017_zonal_2014 <- terra::zonal(lc_2014, pa_2017, fun = "mean")
pa_2017_zonal_2014 <- pa_2017_zonal_2014 %>% mutate(perc_nat_2014 = .[[1]]*100) %>% select(perc_nat_2014)
pa_2017 <- cbind(pa_2017, pa_2017_zonal_2014)

pa_2017_zonal_2018 <- terra::zonal(lc_2018, pa_2017, fun = "mean")
pa_2017_zonal_2018 <- pa_2017_zonal_2018 %>% mutate(perc_nat_2018 = .[[1]]*100) %>% select(perc_nat_2018)
pa_2017 <- cbind(pa_2017, pa_2017_zonal_2018)

pa_2017 <- pa_2017 %>% mutate(perc_change = perc_nat_2014 - perc_nat_2018)

# 2025
pa_2025_zonal_2018 <- terra::zonal(lc_2018, pa_2025, fun = "mean")
pa_2025_zonal_2018 <- pa_2025_zonal_2018 %>% mutate(perc_nat_2018 = .[[1]]*100) %>% select(perc_nat_2018)
pa_2025 <- cbind(pa_2025, pa_2025_zonal_2018)

pa_2025_zonal_2022 <- terra::zonal(lc_2025, pa_2025, fun = "mean")
pa_2025_zonal_2022 <- pa_2025_zonal_2022 %>% mutate(perc_nat_2022 = .[[1]]*100) %>% select(perc_nat_2022)
pa_2025 <- cbind(pa_2025, pa_2025_zonal_2022)

pa_2025 <- pa_2025 %>% mutate(perc_change = perc_nat_2018 - perc_nat_2022)

# LC effectiveness rules:
#  - >10% change: poor
#  - <10% natural: poor
#  - 5-10% change: fair
#  - 10-60% natural: fair
#  - everything else: good

pa_2017 <- pa_2017 %>% mutate(lc_effectiveness = case_when(perc_change >= 10 ~ 1,
                                                           perc_nat_2018 <=10 ~ 1,
                                                           perc_change >=5 & perc_change <10 ~ 2,
                                                           perc_nat_2018 > 10 & perc_nat_2018 <= 60 ~ 2,
                                                           TRUE ~ 3))

pa_2025 <- pa_2025 %>% mutate(lc_effectiveness = case_when(perc_change >= 10 ~ 1,
                                                           perc_nat_2022 <=10 ~ 1,
                                                           perc_change >=5 & perc_change <10 ~ 2,
                                                           perc_nat_2022 > 10 & perc_nat_2022 <= 60 ~ 2,
                                                           TRUE ~ 3))

# ---- Expert contributions -----------

# For 2017 it is derived from the 2018 PA layer
# First the 2018 PA layer's general effectiveness is converted to raster

pa_2018 <- pa_2018 %>% mutate(effectiveness = if_else(effectiveness == 0, 3, effectiveness))
effectiveness_2018 <- terra::rasterize(pa_2018, lc_2014, field = "effectiveness", fun = "max", background = 3)

pa_2017_zonal_2018_effectiveness <- terra::zonal(effectiveness_2018, pa_2017, fun = "mean")
pa_2017_zonal_2018_effectiveness <- pa_2017_zonal_2018_effectiveness %>%
                                    mutate(expert_effectiveness = case_when(effectiveness < 1.5 ~ 1,
                                                                            effectiveness >= 1.5 & effectiveness < 2.5 ~ 2,
                                                                            effectiveness >= 2.5 ~ 3,
                                                                            TRUE ~ 3)) %>% select(expert_effectiveness)

pa_2017 <- cbind(pa_2017, pa_2017_zonal_2018_effectiveness)

# ---- Determine overall effectiveness ----------

# Use pivot longer to find the lowest score per PA ID, cluster, and section combo

pa_2017_df <- as.data.frame(pa_2017)
pa_2017_df <- pa_2017_df %>% select(Cluster_ID, PA_ID, PA_Sec_ID, aip_effectiveness, lc_effectiveness, expert_effectiveness)
pa_2017_df <- pa_2017_df %>% tidyr::pivot_longer(cols = c("aip_effectiveness", "lc_effectiveness", "expert_effectiveness"),
                                                 names_to = "effectiveness_category", values_to = "effectiveness")

general_effectiveness_2017 <- pa_2017_df %>% group_by(Cluster_ID, PA_ID, PA_Sec_ID) %>% 
                                              summarise(general_effectiveness = min(effectiveness)) %>% ungroup()

pa_2017 <- left_join(pa_2017, general_effectiveness_2017, by = c("Cluster_ID", "PA_ID", "PA_Sec_ID"))

# ---- Add effectiveness notes from 2018 -----------

# Convert pa data to points (less messy intersections)
pa_2017_points <- pa_2017 %>% select(Cluster_ID, PA_ID, PA_Sec_ID, PA_name, expert_effectiveness)
pa_2017_points <- terra::centroids(pa_2017_points, inside = TRUE)
pa_2018_notes <- pa_2018 %>% select(pa_name, effectiveness, effectiveness_notes)
pa_2017_points_with_2018_notes <- terra::intersect(pa_2017_points, pa_2018_notes)

# Clean up intersected data 
pa_2017_points_with_2018_notes <- as.data.frame(pa_2017_points_with_2018_notes)
pa_2017_points_with_2018_notes <- pa_2017_points_with_2018_notes %>% tidyr::drop_na(effectiveness_notes)
pa_2017_points_with_2018_notes <- pa_2017_points_with_2018_notes %>% 
                                  mutate(expert_effectiveness = if_else(PA_name == pa_name & 
                                                  expert_effectiveness != effectiveness, effectiveness, expert_effectiveness))
pa_2017_points_with_2018_notes <- pa_2017_points_with_2018_notes %>% 
                                  filter(effectiveness == expert_effectiveness) %>% 
                                  select(Cluster_ID, PA_ID, PA_Sec_ID, expert_effectiveness, effectiveness_notes) %>% 
                                  rename(updated_expert_effectiveness = expert_effectiveness)

# Join to PA data
pa_2017 <- left_join(pa_2017, pa_2017_points_with_2018_notes, by = c("Cluster_ID", "PA_ID", "PA_Sec_ID"))

# Fix expert effectiveness & general effectiveness and drop extra columns
pa_2017 <- pa_2017 %>% 
            mutate(expert_effectiveness = if_else(!is.na(updated_expert_effectiveness), 
                                                  updated_expert_effectiveness, expert_effectiveness))

pa_2017 <- pa_2017 %>% 
            mutate(general_effectiveness = if_else(general_effectiveness > expert_effectiveness, expert_effectiveness, general_effectiveness))

pa_2017 <- pa_2017 %>% select(!(updated_expert_effectiveness))    

# ---- Add livestock, poaching and elephants data from 2018 ------

livestock_2018 <- terra::rasterize(pa_2018, lc_2014, field = "livestock", fun = "max", background = 0)
pa_2017_zonal_livestock <- terra::zonal(livestock_2018, pa_2017, fun = "mean")

pa_2017_zonal_livestock <- pa_2017_zonal_livestock %>%
                            mutate(livestock = if_else(livestock >0.5, 1, 0))

pa_2017 <- cbind(pa_2017, pa_2017_zonal_livestock)

poaching_2018 <- terra::rasterize(pa_2018, lc_2014, field = "poaching", fun = "max", background = 0)
pa_2017_zonal_poaching <- terra::zonal(poaching_2018, pa_2017, fun = "mean")

pa_2017_zonal_poaching <- pa_2017_zonal_poaching %>% 
                          mutate(poaching = if_else(poaching >0.5, 1, 0))

pa_2017 <- cbind(pa_2017, pa_2017_zonal_poaching)

elephants_2018 <- terra::rasterize(pa_2018, lc_2014, field = "elephants", fun = "max", background = 0)
pa_2017_zonal_elephants <- terra::zonal(elephants_2018, pa_2017, fun = "mean")

pa_2017_zonal_elephants <- pa_2017_zonal_elephants %>%
                              mutate(elephants = if_else(elephants >0.5, 1, 0))

pa_2017 <- cbind(pa_2017, pa_2017_zonal_elephants)

# ---- save pa layer for archive ---------------

gpkg_write(pa_2017, destfile = "pas-2017-2025-assessment.gpkg", table_name = "pas-2017", overwrite = TRUE, NoData = NA)

# ---- pass data to paeffectiveness table in database ---------

# establish database connection 
source("db-connection.R")

# 2018 assessment pa data
paeffectiveness_2018 <- as.data.frame(pa_2018)
paeffectiveness_2018 <- paeffectiveness_2018 %>% mutate(effectiveness = as.integer(effectiveness))

dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmppaeffectiveness_2018"), 
             value = paeffectiveness_2018, overwrite = TRUE)

query_insert_2018_effectiveness <- "INSERT INTO speciesdata.tblpaeffectiveness(
    assessment_id,
    pa_id,
	pa_section_id,
	pa_cluster_id,
	pa_name,
	pa_type,
	general_effectiveness,
	effectiveness_notes,
	livestock,
	poaching,
	elephants)
SELECT
  1,
  pa_id,
  0,
  cluster_id,
  pa_name,
  pa_type,
  effectiveness,
  effectiveness_notes,
  livestock,
  poaching,
  elephants
FROM speciesdata.tmppaeffectiveness_2018"

dbExecute(db, query_insert_2018_effectiveness)
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmppaeffectiveness_2018")

# 2017 pas from 2025 assessment
paeffectiveness_2017 <- as.data.frame(pa_2017)
paeffectiveness_2017 <- paeffectiveness_2017 %>% rename(pa_id = PA_ID, pa_cluster_id = Cluster_ID, pa_section_id = PA_Sec_ID, pa_name = PA_name,
                                                        pa_type = PA_type)
paeffectiveness_2017 <- paeffectiveness_2017 %>% mutate(pa_cluster_id = as.integer(pa_cluster_id),
                                                        pa_id = as.integer(pa_id),
                                                        pa_section_id = as.integer(pa_section_id),
                                                        general_effectiveness = as.integer(general_effectiveness),
                                                        livestock = as.integer(livestock),
                                                        poaching = as.integer(poaching),
                                                        elephants = as.integer(elephants))

paeffectiveness_2017 <- paeffectiveness_2017 %>% select(pa_cluster_id, pa_id, pa_section_id, pa_name, pa_type, general_effectiveness,
                                                        effectiveness_notes, livestock, poaching, elephants)
paeffectiveness_2017 <- distinct(paeffectiveness_2017)

dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmppaeffectiveness_2017"), 
             value = paeffectiveness_2017, overwrite = TRUE)

query_insert_2017_effectiveness <- "INSERT INTO speciesdata.tblpaeffectiveness(
    assessment_id,
    pa_id,
	pa_section_id,
	pa_cluster_id,
	pa_name,
	pa_type,
	general_effectiveness,
	effectiveness_notes,
	livestock,
	poaching,
	elephants)
SELECT
  3,
  pa_id,
  pa_section_id,
  pa_cluster_id,
  pa_name,
  pa_type,
  general_effectiveness,
  effectiveness_notes,
  livestock,
  poaching,
  elephants
FROM speciesdata.tmppaeffectiveness_2017"

dbExecute(db, query_insert_2017_effectiveness)
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmppaeffectiveness_2017")

# ---- PA effectiveness expert contributions for 2025 assessment ------------------

# For 2025, expert contributions were collected by PA ID. This represents much more in depth understanding of PA effectiveness
# than what was available for the 2017 assessment. Expert contributions were processed using 
# process-2025-pa-effectiveness-expert-contributions.R. At this stage this data is in a temporary table in the PLAD, and needs to be
# joined to the spatial data

expert_effectiveness_2025 <- dbGetQuery(db, "SELECT * from speciesdata.tmppaexperteffectiveness_2025")
expert_effectiveness_2025_join <- expert_effectiveness_2025 %>% select(pa_id, pa_section_id, pa_cluster_id, expert_effectiveness)

pa_2025 <- left_join(pa_2025, expert_effectiveness_2025_join, by = c("PA_ID" = "pa_id", "PA_Sec_ID" = "pa_section_id", 
                                                                     "Cluster_ID" = "pa_cluster_id"))

# For the 2017 PAs, the worst effectiveness out of the 3 ratings was used. This was done because the "expert" data was as much a guess
# as anything else for most PAs. However, for 2025, expert rating takes preference over other scores. Other scores are only used where
# experts did not comment on the PA

pa_2025 <- pa_2025 %>% mutate(general_effectiveness = as.integer(case_when(!is.na(expert_effectiveness) ~ expert_effectiveness,
                                                                          lc_effectiveness < aip_effectiveness ~ lc_effectiveness,
                                                                          aip_effectiveness < lc_effectiveness ~ aip_effectiveness,
                                                                          aip_effectiveness == lc_effectiveness ~ aip_effectiveness,
                                                                          TRUE ~ NA)))

# Also join the other data from the expert contributions
expert_effectiveness_2025_join <- expert_effectiveness_2025 %>% select(pa_id, pa_section_id, pa_cluster_id, 
                                                                       effectiveness_notes, livestock, poaching, elephants)

pa_2025 <- left_join(pa_2025, expert_effectiveness_2025_join, by = c("PA_ID" = "pa_id", "PA_Sec_ID" = "pa_section_id", 
                                                                     "Cluster_ID" = "pa_cluster_id"))

# Because not all the PAs have expert comments, some tidying needs to be done
pa_2025 <- pa_2025 %>% mutate(livestock = as.integer(if_else(is.na(livestock), 0, livestock)),
                              poaching = as.integer(if_else(is.na(poaching), 0, poaching)),
                              elephants = as.integer(if_else(is.na(elephants), 0, elephants)))

# ---- Compare 2017 and 2025 effectiveness ---------

# For assessment purposes (and also a general data sense check) do a spatial intersect of 2017 effectiveness with 2025 effectiveness
# And compare differences

# Assuming the script was closed to process the 2025 expert comments, re-load the processed 2017 layer
pa_2017_conn <- gpkg_read(file.path(pa_file_path, "pas-2017-2025-assessment.gpkg"), connect = TRUE)
pa_2017 <- gpkg_vect(pa_2017_conn, "pas-2017")
pa_2017 <- project(pa_2017, pl_projection)

# Create a point file from pa_2025 to join attributes from pa_2017 (results in a cleaner intersect)

pa_2025_points <- pa_2025 %>% select(Cluster_ID, PA_ID, PA_Sec_ID)
pa_2025_points <- terra::centroids(pa_2025_points, inside = TRUE)
pa_2017_effectiveness <- pa_2017 %>% select(general_effectiveness)
pa_2025_points_with_2017_effectiveness <- terra::intersect(pa_2025_points, pa_2017_effectiveness)
pa_2025_points_with_2017_effectiveness <- as.data.frame(pa_2025_points_with_2017_effectiveness)
pa_2025_points_with_2017_effectiveness <- pa_2025_points_with_2017_effectiveness %>% rename(effectiveness_2017 = general_effectiveness)

pa_2025 <- left_join(pa_2025, pa_2025_points_with_2017_effectiveness, by = c("PA_ID", "PA_Sec_ID", "Cluster_ID"))

pa_2025 <- pa_2025 %>% mutate(effectiveness_change = case_when(effectiveness_2017 > general_effectiveness ~ "deterioration",
                                                               effectiveness_2017 < general_effectiveness ~ "improvement",
                                                               effectiveness_2017 == general_effectiveness ~ "no change",
                                                               TRUE ~ NA))
pa_2025_attributes <- as.data.frame(pa_2025)
pa_effectiveness_change_summary <- pa_2025_attributes %>% group_by(effectiveness_change) %>% summarise(count = n())

# Save the processed PA layer
gpkg_write(pa_2025, destfile = "pas-2024-2025-assessment.gpkg", table_name = "pas-2024", overwrite = TRUE, NoData = NA)

# After some checks & fixes, need to import the fixed effectiveness data from the gpkg
pa_2025_conn <- gpkg_read(file.path(pa_file_path, "pas-2024-2025-assessment.gpkg"), connect = TRUE)
pa_2025 <- gpkg_vect(pa_2025_conn, "pas-2024")
pa_2025 <- project(pa_2025, pl_projection)

# Convert back to df
pa_2025_attributes <- as.data.frame(pa_2025)

# --- Pass 2025 PA effectiveness data to db -----------

# Clean up the data
pa_2025_attributes <- pa_2025_attributes %>% select(PA_ID, PA_Sec_ID, Cluster_ID, PA_name, PA_type, general_effectiveness,
                                                    effectiveness_notes, livestock, poaching, elephants)

pa_2025_attributes <- pa_2025_attributes %>% rename(pa_id = PA_ID, pa_cluster_id = Cluster_ID, pa_section_id = PA_Sec_ID, 
                                                    pa_name = PA_name, pa_type = PA_type)

pa_2025_attributes <- pa_2025_attributes %>% mutate(pa_cluster_id = as.integer(pa_cluster_id),
                                                    pa_id = as.integer(pa_id),
                                                    pa_section_id = as.integer(pa_section_id))

# Send to DB

dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmppaeffectiveness_2024"), 
             value = pa_2025_attributes, overwrite = TRUE)

query_insert_2024_effectiveness <- "INSERT INTO speciesdata.tblpaeffectiveness(
    assessment_id,
    pa_id,
	pa_section_id,
	pa_cluster_id,
	pa_name,
	pa_type,
	general_effectiveness,
	effectiveness_notes,
	livestock,
	poaching,
	elephants)
SELECT
  2,
  pa_id,
  pa_section_id,
  pa_cluster_id,
  pa_name,
  pa_type,
  general_effectiveness,
  effectiveness_notes,
  livestock,
  poaching,
  elephants
FROM speciesdata.tmppaeffectiveness_2024"

dbExecute(db, query_insert_2024_effectiveness)
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmppaeffectiveness_2024")
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmppaexperteffectiveness_2025")

# ---- close database connection --------------------
dbDisconnect(db)

