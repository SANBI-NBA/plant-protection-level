# This script processes and combines expert comments on PA effectiveness received from various
# separate files into a single output - this output is added to the 2025 PA layer as expert effectiveness
# in the script set-pa-effectiveness-2025-assessment.R
# This is intended as a permanent record of the process for archiving purposes, rather than a reusable script

# ---- load libraries ---------------------------------
library(dplyr)     # For general data wrangling
library(tidyr)     # For data cleaning
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data
library(utf8)      # Character encoding gives issues when passing data to postgresql

# ---- load data -------------------------------------

refpauid <- read.csv("data/effectiveness/refpauid.csv", header = TRUE)
refpas <- read.csv("data/effectiveness/refpanames.csv", header = TRUE)

effectiveness_1 <- read.csv("data/effectiveness/effectiveness_1_combined_sheets_with_source_sanparks.csv", header = TRUE)
effectiveness_2 <- read.csv("data/effectiveness/effectiveness_2_capenature_eastern_region_general.csv", header = TRUE)
effectiveness_3 <- read.csv("data/effectiveness/effectiveness_3_capenature_eastern_region_plants.csv", header = TRUE)
effectiveness_4 <- read.csv("data/effectiveness/effectiveness_4_capenature_western_region_general.csv", header = TRUE)
effectiveness_5 <- read.csv("data/effectiveness/effectiveness_5_all_other_regions.csv", header = TRUE)

# ---- create a renaming vector for changing column names to match PLAD ----
rename_lookup <- tibble(
  new_names = c("pa_name", "pa_type", "pa_id", "pa_section_id", "pa_cluster_id"), 
  old_names = c("PA_name", "PA_type", "PA_ID", "PA_Sec_ID", "Cluster_ID")
)

rename_lookup <- rename_lookup %>% tibble::deframe()

# ---- process effectiveness_1 -------------------------
eff_1_source_categories <- effectiveness_1 %>% group_by(source) %>% summarise(count = n()) %>% ungroup()

# Technically, should look at General and Plants, however, the 3 PAs coded under general are all MPAs, so dropping them
eff_1_processed <- effectiveness_1 %>% filter(source == "Plants")
eff_1_processed <- eff_1_processed %>% filter(Score != "")
# Also filter out some junk records
eff_1_processed <- eff_1_processed %>% filter(`No.record` != "e.g. ")

# Keep only relevant columns and rename them using stored rename_lookup
eff_1_processed <- eff_1_processed %>% select(PA_name, PA_type, PA_ID, PA_Sec_ID, Cluster_ID, PA_UID,
                                              Score, Motivation) %>% rename(!!! rename_lookup)

# Translate text effectiveness to effectiveness_id
eff_1_processed <- eff_1_processed %>% mutate(expert_effectiveness = as.integer(case_when(Score == "Poor" ~ 1,
                                                                                           Score == "Fair" ~ 2,
                                                                                           Score == "Good" ~ 3,
                                                                                TRUE ~ NA)))

# Add specific pressures
eff_1_processed <- eff_1_processed %>% mutate(livestock = as.integer(if_else(stringr::str_detect(Motivation, "grazing"), 1, 0)))
eff_1_processed <- eff_1_processed %>% mutate(poaching = as.integer(if_else(stringr::str_detect(Motivation, "harvesting") | stringr::str_detect(Motivation, "poaching") , 1, 0)))
eff_1_processed <- eff_1_processed %>% mutate(elephants = as.integer(0))

# ---- process effectiveness_2 -------------------------

eff_2_processed <- effectiveness_2 %>% select(PA_name, PA_type, PA_ID, PA_Sec_ID, Cluster_ID, PA_UID,
                                              Score, Motivation) %>% rename(!!! rename_lookup)

eff_2_processed <- eff_2_processed %>% filter(Score != "")
eff_2_processed <- eff_2_processed %>% mutate(expert_effectiveness = as.integer(case_when(Score == "Poor" ~ 1,
                                                                                          Score == "Fair" ~ 2,
                                                                                          Score == "Good" ~ 3,
                                                                                          TRUE ~ NA)))
eff_2_processed <- eff_2_processed %>% mutate(livestock = as.integer(0))
eff_2_processed <- eff_2_processed %>% mutate(poaching = as.integer(if_else(stringr::str_detect(Motivation,"poaching"), 1, 0)))
eff_2_processed <- eff_2_processed %>% mutate(elephants = as.integer(0))

# ---- process effectiveness_3 -------------------------------
# This csv does not contain any contributed data

# ---- process effectiveness_4 -------------------------------

eff_4_processed <- effectiveness_4 %>% filter(`No.record` != "e.g. ")
eff_4_processed <- eff_4_processed %>% filter(`No.record` != "examples")

eff_4_processed <- eff_4_processed %>% select(PA_name, PA_type, PA_ID, PA_Sec_ID, Cluster_ID, PA_UID,
                                                  Score, Motivation) %>% rename(!!! rename_lookup)

eff_4_processed <- eff_4_processed %>% filter(Score != "")
eff_4_processed <- eff_4_processed %>% mutate(expert_effectiveness = as.integer(case_when(Score == "Poor" ~ 1,
                                                                                          Score == "Fair" ~ 2,
                                                                                          Score == "Good" ~ 3,
                                                                                          TRUE ~ NA)))
eff_4_processed <- eff_4_processed %>% mutate(livestock = as.integer(0))
eff_4_processed <- eff_4_processed %>% mutate(poaching = as.integer(if_else(stringr::str_detect(Motivation,"poaching"), 1, 0)))
eff_4_processed <- eff_4_processed %>% mutate(elephants = as.integer(0))

# ---- process effectiveness_5 -------------------------------

# This one needs the most processing (sourced from online submissions)

eff_5_source_categories <- effectiveness_5 %>% group_by(.[[5]]) %>% summarise(count = n()) %>% ungroup()

eff_5_processed <- effectiveness_5 %>% filter(.[[5]] == "General" | .[[5]] == "Plants")

# PA IDs are captured in different ways: attempting to deal with it here
eff_5_processed_uid <- eff_5_processed %>% filter(pa_uid != "all")
eff_5_processed_pa_id <- eff_5_processed %>% filter(pa_uid == "all" & pa_id != "" & pa_id != "all")
eff_5_processed_cluster_id <- eff_5_processed %>% filter(pa_uid == "all" & (pa_id == "" | pa_id == "all") & cluster_id != "")

# All these id fields came out as strings, process to integers
eff_5_processed_uid <- eff_5_processed_uid %>% mutate(pa_uid = stringr::str_replace_all(pa_uid, " ", ""))
eff_5_processed_uid <- eff_5_processed_uid %>% mutate(PA_UID = as.integer(pa_uid))

eff_5_processed_pa_id <- eff_5_processed_pa_id %>% mutate(PA_ID = as.integer(pa_id))
eff_5_processed_cluster_id <- eff_5_processed_cluster_id %>% mutate(Cluster_ID = as.integer(cluster_id))

# Now join the PA IDs reference table to all of them
eff_5_processed_uid <- left_join(eff_5_processed_uid, refpauid, by = "PA_UID")
eff_5_processed_uid <- eff_5_processed_uid %>% tidyr::drop_na(PA_ID)

eff_5_processed_pa_id <- left_join(eff_5_processed_pa_id, refpauid, by = "PA_ID")
eff_5_processed_cluster_id <- left_join(eff_5_processed_cluster_id, refpauid, by = "Cluster_ID")

# Select only the necessary columns
eff_5_processed_uid <- eff_5_processed_uid %>% select(PA_ID, PA_Sec_ID, Cluster_ID, PA.effectiveness.score, Motivation)
eff_5_processed_pa_id <- eff_5_processed_pa_id %>% select(PA_ID, PA_Sec_ID, Cluster_ID, PA.effectiveness.score, Motivation)
eff_5_processed_cluster_id <- eff_5_processed_cluster_id %>% select(PA_ID, PA_Sec_ID, Cluster_ID, PA.effectiveness.score, Motivation)

# Merge all the bits together again
eff_5_processed <- rbind(eff_5_processed_uid, eff_5_processed_pa_id, eff_5_processed_cluster_id)

# Eliminate duplicates
eff_5_processed <- distinct(eff_5_processed)

# Do some final data cleanup
eff_5_processed <- eff_5_processed %>% rename(Score = PA.effectiveness.score)
eff_5_processed <- eff_5_processed %>% mutate(expert_effectiveness = as.integer(case_when(Score == "Poor" ~ 1,
                                                                                          Score == "Fair" ~ 2,
                                                                                          Score == "Good" ~ 3,
                                                                                          TRUE ~ NA)))

eff_5_processed <- eff_5_processed %>% mutate(livestock = as.integer(if_else(stringr::str_detect(Motivation, "grazing"), 1, 0)))
eff_5_processed <- eff_5_processed %>% mutate(poaching = as.integer(if_else(stringr::str_detect(Motivation, "medicinal") | stringr::str_detect(Motivation, "poaching") |  stringr::str_detect(Motivation, "harvesting"), 1, 0)))
eff_5_processed <- eff_5_processed %>% mutate(elephants = as.integer(if_else(stringr::str_detect(Motivation, "elephant"), 1, 0)))


# Check column names and data types match PLAD
eff_5_processed <- eff_5_processed %>% rename("pa_id" = "PA_ID", "pa_section_id" = "PA_Sec_ID", "pa_cluster_id" = "Cluster_ID",
                                              "effectiveness_notes" = "Motivation") %>% select(pa_id, pa_section_id, pa_cluster_id,
                                                                                              expert_effectiveness, effectiveness_notes,
                                                                                              livestock, poaching, elephants)

# ---- Merge and process other effectiveness data to match eff_5_processed ---------

eff_processed <- rbind(eff_1_processed, eff_2_processed, eff_4_processed)

eff_processed <- eff_processed %>% rename("effectiveness_notes" = "Motivation") %>% select(pa_id, pa_section_id, pa_cluster_id,
                                                                                           expert_effectiveness, effectiveness_notes,
                                                                                           livestock, poaching, elephants)
eff_processed <- distinct(eff_processed)
eff_5_processed <- distinct(eff_5_processed)

paexperteffectiveness_2025 <- rbind(eff_processed, eff_5_processed)

# Add PA name
refpas_name <- refpas %>% select(PA_ID, PA_name) %>% rename("pa_id" = "PA_ID", "pa_name" = "PA_name")

paexperteffectiveness_2025 <- inner_join(refpas_name, paexperteffectiveness_2025, by = "pa_id")
paexperteffectiveness_2025 <- distinct(paexperteffectiveness_2025)

# Check for duplicates

paexperteffectiveness_2025_duplicates <- paexperteffectiveness_2025 %>% group_by(pa_id, pa_section_id, pa_cluster_id) %>% summarise(duplicated = n())
paexperteffectiveness_2025_duplicates <- paexperteffectiveness_2025_duplicates %>% mutate(duplicated = if_else(duplicated == 1, 0, 1))

paexperteffectiveness_2025 <- left_join(paexperteffectiveness_2025, paexperteffectiveness_2025_duplicates, by = c("pa_id", "pa_section_id", "pa_cluster_id"))

# ---- Pass data to database --------

# establish database connection 
source("db-connection.R")

# Check character encoding
paexperteffectiveness_2025 <- paexperteffectiveness_2025 %>% mutate(pa_name = as_utf8(pa_name, normalize = TRUE))
paexperteffectiveness_2025 <- paexperteffectiveness_2025 %>% mutate(effectiveness_notes = as_utf8(effectiveness_notes, normalize = TRUE))
paexperteffectiveness_2025 <- paexperteffectiveness_2025 %>% mutate(livestock = as.integer(livestock))

dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmppaexperteffectiveness_2025"), 
             value = paexperteffectiveness_2025, overwrite = TRUE)

# After this, the expert effectiveness data was manually checked and cleaned up of duplicates
