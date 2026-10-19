# Script for determining reasons for protection level changes between different time points
# This data was used for producing Fig 4 in protection level paper
# But it is also useful for reporting

library(dbx)               # For postgres database connection
library(DBI)               # For interacting with postgres data
library(dplyr)             # For general data wrangling
library(terra)             # For working with spatial data
library(gpkg)              # For reading spatial data in gpkg format
library(tidyr)             # For data cleaning

# ---- Setup -----

# Set assessment id
var_current_assessment_id <- 2

# Connect to the database
source("db-connection.R")

# Path for PA data (which has comparative effectiveness)
pa_file_path <- Sys.getenv("PA_DATA_PATH")

# ---- Get data -----

# Get species pl data from database

assessment_query <- paste0("SELECT s.species_id, s.taxon, a.number_pas, 
  a.protectioncategory_no_effectiveness, pl1.protectionweight as protectionweight_no_effectiveness,
  a.protectioncategory, pl2.protectionweight,
  t.timepoint_year, a.assessment_id, a.timepoint_id
FROM speciesdata.tblplassessment a
INNER JOIN speciesdata.tblspecies s ON a.species_id = s.species_id
INNER JOIN speciesdata.refassessmenttimepoints t ON a.assessment_id = t.assessment_id AND a.timepoint_id = t.timepoint_id
INNER JOIN speciesdata.refprotectionlevel pl1 ON a.protectioncategory_no_effectiveness = pl1.protectioncategory
INNER JOIN speciesdata.refprotectionlevel pl2 ON a.protectioncategory = pl2.protectioncategory
WHERE a.assessment_id = ", var_current_assessment_id)

assessment <- dbGetQuery(db, assessment_query)

# Get speciex x PA data from database

query_species_pa <- paste0("SELECT assessment_id,
                                    timepoint_id,
                                    species_id, 
                                    pa_id, 
                                    pa_section_id, 
                                    pa_cluster_id, 
                                    pa_effectiveness
                            FROM speciesdata.tblspeciesinpa
                            WHERE assessment_id = ", var_current_assessment_id,
                           " AND (presence_pa = 1 OR presence_cluster = 1)")

species_pa <- dbGetQuery(db, query_species_pa)

# The 2024 PA layer has a column indicating which PAs changed effectiveness since 2017

pa_layer_name <- "pas-2024"
pa_file <- "pas-2024-2025-assessment.gpkg"
pa_file_path <- file.path(pa_file_path, pa_file)

pa_conn <- gpkg_read(pa_file_path, connect = TRUE)
pa <- gpkg_vect(pa_conn, pa_layer_name)

# Get the attributes of this layer
pa_attributes <- as.data.frame(pa)

pa_effectiveness_change <- pa_attributes %>% select(PA_ID, PA_Sec_ID, Cluster_ID, effectiveness_change)

# The data only applies to 2024 PA ids
# Filter and join

species_pa <- species_pa %>% filter(timepoint_id == 3)
species_pa <- left_join(species_pa, pa_effectiveness_change, 
                        by = c("pa_id" = "PA_ID",
                               "pa_section_id" = "PA_Sec_ID",
                               "pa_cluster_id" = "Cluster_ID"))

# ---- Identify species with PL changes between time points -----

plant_pl_assessment <- assessment %>% select(species_id, taxon, timepoint_year, 
                                          number_pas, protectioncategory_no_effectiveness,
                                          protectionweight_no_effectiveness,
                                          protectioncategory, protectionweight) %>% 
  tidyr::pivot_wider(names_from = timepoint_year,
                     values_from = c(number_pas, protectioncategory_no_effectiveness,
                                     protectionweight_no_effectiveness,
                                     protectioncategory, protectionweight)) 
  
category_changes <- plant_pl_assessment %>% filter(protectioncategory_2017 != protectioncategory_2024)

# ---- Classify changes according to direction of change and reason for change ----

# Add data on number of PAs with effectiveness changes

pa_effectiveness_change_summary <- species_pa %>% 
                                  filter(species_id %in% category_changes$species_id) %>% 
                                  group_by(species_id, effectiveness_change) %>% 
                                  summarise(no_pas = n()) %>% 
                  tidyr::pivot_wider(names_from = effectiveness_change,
                                     values_from = no_pas) %>% 
                  mutate(
                        across(where(is.numeric), tidyr::replace_na, 0))

category_changes <- left_join(category_changes, pa_effectiveness_change_summary,
                              by = "species_id")

# Sort species into improved and decreased PL

category_changes <- category_changes %>% 
                      mutate(pl_change = if_else(protectionweight_2017 > protectionweight_2024, "improved", "declined"))

# Assign reasons for change:
#  - declined: deproclamations = fewer PAs in 2024 than 2017, deterioration = 0
#  - declined: effectiveness = same PAs in 2024 than 2017, deterioration != 0 / difference in PL with effectiveness and without effectiveness in 2024
#  - declined: deproclamations + effectiveness = fewer PAs in 2024 than 2017, deterioration != 0
#  - improved: expansion = more PAs in 2024 than 2017, improvement = 0
#  - improved: effectiveness = same PAs in 2024 than 2017, improvement != 0
#  - improved: effectiveness + expansion = more PAs in 2024 than 2017, improvement != 0

category_changes <- category_changes %>% 
                        mutate(reason_for_change = case_when(pl_change == "declined" & 
                                                               number_pas_2024 < number_pas_2017 &
                                                               deterioration == 0 ~ "deproclamations",
                                                             pl_change == "declined" & 
                                                               number_pas_2024 >= number_pas_2017 & 
                                                               deterioration != 0 ~ "effectiveness",
                                                             pl_change == "declined" & 
                                                               number_pas_2024 >= number_pas_2017 &
                                                               protectionweight_no_effectiveness_2024 < protectionweight_2024 ~ "effectiveness",
                                                             pl_change == "declined" & 
                                                               number_pas_2024 < number_pas_2017 & 
                                                               deterioration != 0 ~ "deproclamations + effectiveness",
                                                             pl_change == "improved" & 
                                                               number_pas_2024 > number_pas_2017 & 
                                                               improvement == 0 ~ "expansion",
                                                             pl_change == "improved" & 
                                                               number_pas_2024 <= number_pas_2017 & 
                                                               improvement != 0 ~ "effectiveness",
                                                             pl_change == "improved" & 
                                                               number_pas_2024 <= number_pas_2017 &
                                                               protectionweight_no_effectiveness_2017 < protectionweight_2017 &
                                                               protectionweight_no_effectiveness_2024 == protectionweight_2024 ~ "effectiveness",
                                                             pl_change == "improved" & 
                                                               number_pas_2024 > number_pas_2017 & 
                                                               improvement != 0 ~ "expansion + effectiveness",
                                                             TRUE ~ NA))


join_category_changes <- category_changes %>% select(species_id, pl_change, reason_for_change)

plant_pl_assessment <- left_join(plant_pl_assessment, join_category_changes, by = "species_id")
plant_pl_assessment <- plant_pl_assessment %>% mutate(pl_change = replace_na(pl_change, "no change"))

write.csv(plant_pl_assessment, file = "plant_pl_assessment_2025.csv", row.names = FALSE)

# --- Summarise reasons for change ----

pl_change <- c("declined", "declined", "declined", "improved", "improved", "improved")
reason_for_change <- c("deproclamations", "effectiveness",  "deproclamations + effectiveness", "expansion", "effectiveness", "expansion + effectiveness")

rfc_summary_table <- data.frame(pl_change, reason_for_change)

category_changes_summary <- category_changes %>% 
                                group_by(pl_change, reason_for_change) %>% 
                                summarise(number_of_species = n())

rfc_summary_table <- left_join(rfc_summary_table, category_changes_summary,
                               by = c("pl_change", "reason_for_change")) %>% 
                          mutate(
                            across(where(is.numeric), tidyr::replace_na, 0))
  
write.csv(rfc_summary_table, file = "rfc_summary_table.csv", row.names = FALSE)

# --- Estimate how many under-represented species increased habitat protection ----

habitat_protection_query <- paste0("select
  a.species_id,
  a.timepoint_id,
  sum(p.area_habitat_pa) as total_habitat_protected
from speciesdata.tblplassessment a 
inner join speciesdata.tblspeciesinpa p
on a.assessment_id = p.assessment_id
and a.timepoint_id = p.timepoint_id
and a.species_id = p.species_id
    where a.assessment_id = ", var_current_assessment_id, 
    " group by a.species_id, a.timepoint_id"
)

habitat_protection_changes <- dbGetQuery(db, habitat_protection_query)

habitat_protection_changes <- left_join(assessment, habitat_protection_changes,
                                        by = c("species_id", "timepoint_id"))

habitat_protection_changes_comparison <- habitat_protection_changes %>% 
                                          select(species_id, taxon, timepoint_year, 
                                             protectioncategory, protectionweight,
                                             total_habitat_protected) %>% 
  tidyr::pivot_wider(names_from = timepoint_year,
                     values_from = c(protectioncategory, protectionweight, total_habitat_protected))

# We are only interested in changes for species that were not well protected in 2017
habitat_protection_changes_comparison <- habitat_protection_changes_comparison %>% 
                                            filter(protectioncategory_2017 != "Well Protected")

# Now we want to know where total habitat protected increased
habitat_protection_increases <- habitat_protection_changes_comparison %>% 
                                    filter(total_habitat_protected_2024 > total_habitat_protected_2017)