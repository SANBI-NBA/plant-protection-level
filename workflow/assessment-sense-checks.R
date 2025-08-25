# Script for checking logical consistency of Protection Level assessment results

# ---- load libraries ---------------------------------
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data
library(dplyr)     # For general data wrangling

# ---- set up reusable variables for current assessment -----------------------------------------------

# This means that the script can be reused for future assessments without needing to change anything other than
# these variables.
var_current_assessment_id <- 2
var_current_assessment_year <- 2025

# ---- establish database connection -----

# Note that this assumes a script called db-connection.R is present in the same folder as this script
# If the database is in a Docker container, it assumes that the container is running
# The database login information is contained in an .Renviron file (not part of the repo)

source("db-connection.R")

# ---- get information about time points for the assessment from the PLAD ------

query_time_points <- paste0("SELECT timepoint_id, timepoint_year FROM speciesdata.refassessmenttimepoints WHERE assessment_id = ", var_current_assessment_id)
assessment_time_points <- dbGetQuery(db, query_time_points)

# ---- get species targets -----

query_targets <- paste0("SELECT t.species_id, s.taxon, s.rl_status, s.resilient, t.population_status, a.abundance_category as abundance,
                            t.area_target, t.population_target, t.subpopulation_target
                         FROM speciesdata.tbltargets t 
                         INNER JOIN speciesdata.tblspecies s ON t.species_id = s.species_id
                         INNER JOIN speciesdata.refabundance a ON t.abundance = a.abundance_id
                         WHERE t.assessment_id = ", var_current_assessment_id)

targets <- dbGetQuery(db, query_targets)

# ---- get assessment data ----

query_assessments <- paste0("SELECT s.species_id, s.taxon, a.number_pas, a.total_habitat_pas, a.total_population_pas, a.total_localities_pas,
  a.protectionscore_no_effectiveness, a.protectioncategory_no_effectiveness, pl1.protectionweight as protectionweight_no_effectiveness,
  a.protectionscore, a.protectioncategory, pl2.protectionweight, t.timepoint_year
FROM speciesdata.tblplassessment a 
INNER JOIN speciesdata.tblspecies s ON a.species_id = s.species_id
INNER JOIN speciesdata.refassessmenttimepoints t ON a.assessment_id = t.assessment_id AND a.timepoint_id = t.timepoint_id
INNER JOIN speciesdata.refprotectionlevel pl1 ON a.protectioncategory_no_effectiveness = pl1.protectioncategory
INNER JOIN speciesdata.refprotectionlevel pl2 ON a.protectioncategory = pl2.protectioncategory
WHERE a.assessment_id = ", var_current_assessment_id)

assessments <- dbGetQuery(db, query_assessments)

# ---- get species x pa data ----

query_species_pa <- paste0("SELECT assessment_id, timepoint_id, species_id, presence_pa, presence_cluster,
                                count(distinct (pa_id)) as number_of_pas, sum(area_habitat_pa) as sum_habitat,
                                sum(population_pa) as sum_population, sum(localities_pa) as total_localities
                            FROM speciesdata.tblspeciesinpa WHERE assessment_id = ", var_current_assessment_id, 
                            " GROUP BY assessment_id, timepoint_id, species_id, presence_pa, presence_cluster")

species_pa <- dbGetQuery(db, query_species_pa)

#############################################################
#                                                           #
#                       DATA CHECKS                         #
#                                                           #
#############################################################

# targets should contain 900 records (number of species in the sample)
# assessments should contain 900*n time points in assessment

# ---- Check 1: status changes due to effectiveness impacts ----
# Considering effectiveness can only cause a species to be in the same category, or a worse category

effectiveness_impacts <- assessments %>% group_by(timepoint_year, protectioncategory_no_effectiveness, protectioncategory) %>% 
                                          summarise(number_of_species = n()) %>% 
                                          filter(protectioncategory_no_effectiveness != protectioncategory) %>% 
                                          tidyr::pivot_wider(names_from = timepoint_year, values_from = number_of_species) %>% 
                                          mutate(protectioncategory_no_effectiveness = as.character(protectioncategory_no_effectiveness),
                                                    protectioncategory = as.character(protectioncategory)) %>%
                                          janitor::adorn_totals(name = "Total species")

# Most species change down by one category, but a few species go from Well Protected to Poorly Protected
# Check what they are

species_wp_to_pp <- assessments %>% filter(protectioncategory_no_effectiveness == "Well Protected",
                                           protectioncategory == "Poorly Protected")

species_wp_to_pp <- left_join(species_wp_to_pp, targets, by = "species_id")


# ---- Check 2: Population categories that cannot be Well Protected ----

# If these species came out as Well Protected there is something wrong with the data
population_status <- targets %>% select(species_id, taxon, population_status)
category_checks <- assessments %>% select(species_id, protectioncategory, timepoint_year)
category_checks <- left_join(category_checks, population_status, by = "species_id")

# Population status 2,3, and 4 cannot be Well Protected
category_checks_not_well_protected <- category_checks %>% filter(population_status %in% c(2,3,4)) %>% 
                                      group_by(protectioncategory) %>% summarise(number_of_species = n())

# Special checks on category 6
# Threatened species with highly restricted ranges (population size suspected to be <10 000)
# If any of these species are well protected, the effectiveness of the PAs where they occur may be wrong
category_checks_6 <- category_checks %>% filter(population_status == 6)


# ---- Check 3: Distribution of PL categories ----

protection_summary_by_category <- assessments %>% group_by(timepoint_year, protectioncategory) %>% 
                                                  summarise(number_of_species = n()) %>% 
                                                  mutate(percentage_of_species = (number_of_species/900)*100) %>% 
                                                  tidyr::pivot_wider(names_from = timepoint_year, 
                                                                     values_from = c(number_of_species, percentage_of_species))  

# ---- Check 4: Species that changed category between 2017 and 2024 ---
category_changes <- assessments %>% select(species_id, timepoint_year, protectioncategory, protectionweight) %>% 
                                    tidyr::pivot_wider(names_from = timepoint_year, values_from = c(protectioncategory, protectionweight)) %>% 
                                    filter(protectioncategory_2017 != protectioncategory_2024)

category_changes_2017_2024_summary <- category_changes %>% 
                                      group_by(protectioncategory_2017, protectioncategory_2024,
                                               protectionweight_2017, protectionweight_2024) %>% 
                                      summarise(number_of_species = n()) %>% 
                                                            mutate(change = if_else(protectionweight_2017 > protectionweight_2024, "improvement", "deterioration")) %>% 
                                      ungroup() %>% 
                                      select(protectioncategory_2017, protectioncategory_2024, number_of_species, change)

# ---- Check 5: Reasons for species with deteriorating PL ----

reasons_for_deterioration <- category_changes %>% filter(protectionweight_2017 < protectionweight_2024)
deterioration_data <- assessments %>% filter(species_id %in% reasons_for_deterioration$species_id) %>% 
                                      select(species_id, taxon, number_pas, total_habitat_pas, total_population_pas,
                                             total_localities_pas, protectionscore_no_effectiveness, protectioncategory_no_effectiveness,
                                             protectionscore, protectioncategory, timepoint_year) %>% 
                                      tidyr::pivot_wider(names_from = timepoint_year, 
                                                         values_from = c(number_pas, total_habitat_pas, total_population_pas,
                                                                         total_localities_pas, protectionscore_no_effectiveness, protectioncategory_no_effectiveness,
                                                                         protectionscore, protectioncategory))

# ---- Check 6: How did data and target changes between previous and current assessment impact results ----

query_assessment_2018 <- "SELECT species_id, number_pas, total_habitat_pas, protectioncategory, protectionrationale
                          FROM speciesdata.tblplassessment WHERE assessment_id = 1"

assessment_2018 <- dbGetQuery(db, query_assessment_2018)

assessment_2017_backcasted <- assessments %>% filter(timepoint_year == 2017) %>% 
                                              select(species_id, number_pas, total_habitat_pas, protectioncategory) %>% 
                                              mutate(protectionrationale = "Not yet defined")

assessment_2018 <- assessment_2018 %>% mutate(assessment = "original")
assessment_2017_backcasted <- assessment_2017_backcasted %>% mutate(assessment = "backcasted")

compare_assessments_original_backcasted <- rbind(assessment_2018, assessment_2017_backcasted)
compare_assessments_original_backcasted <- compare_assessments_original_backcasted %>% 
                                            tidyr::pivot_wider(names_from = assessment, 
                                            values_from = c(number_pas, total_habitat_pas, protectioncategory, protectionrationale)) %>% 
                                            filter(protectioncategory_original != protectioncategory_backcasted)

# Classify differences

# Habitat protected - same or different?
compare_assessments_original_backcasted <- compare_assessments_original_backcasted %>%
                                            mutate(habitat_difference = abs(total_habitat_pas_original - total_habitat_pas_backcasted))
compare_assessments_original_backcasted <- compare_assessments_original_backcasted %>% 
                                              mutate(rfc = case_when(number_pas_original == number_pas_backcasted |
                                                                       habitat_difference < 10 ~ "target",
                                                                     is.na(total_habitat_pas_backcasted) ~ "method",
                                                                     TRUE ~ "data"))

summary_differences <- compare_assessments_original_backcasted %>% group_by(rfc) %>% summarise(number_of_species = n())

###################################################################
#                                                                 #
#                       OUTPUT ASSESSMENT                         #
#                                                                 #
###################################################################

# When all checks have passed, requery assessment data and write to CSV

query_assessment <- paste0("SELECT s.taxon, a.protectionscore_no_effectiveness, a.protectioncategory_no_effectiveness,
                                a.protectionscore, a.protectioncategory, t.timepoint_year
                            FROM speciesdata.tblplassessment a
                            INNER JOIN speciesdata.tblspecies s ON a.species_id = s.species_id
                            INNER JOIN speciesdata.refassessmenttimepoints t ON a.assessment_id = t.assessment_id 
                              AND a.timepoint_id = t.timepoint_id
                            WHERE a.assessment_id = ", var_current_assessment_id, " ORDER by s.taxon")

assessment <- dbGetQuery(db, query_assessment)
assessment <- assessment %>% tidyr::pivot_wider(names_from = timepoint_year, 
                                                values_from = c(protectionscore_no_effectiveness, protectioncategory_no_effectiveness,
                                                                protectionscore, protectioncategory))

write.csv(assessment, file = paste0("plant-pl-assessment-", var_current_assessment_year, ".csv"), row.names = FALSE)