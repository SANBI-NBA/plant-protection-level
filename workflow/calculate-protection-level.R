# Workflow for calculating plant species protection level
# This script assumes that species data (occurrence records and suitable habitat maps)
# have been intersected with PA layer, and results added to tblspeciesinpa in the PLAD
# and species-specific protected area effectiveness rules have been applied (apply-pa-effectiveness-x-species.R)
# This script processes data already in the PLAD and does not require any external inputs
# data is read from the PLAD (requires a database connection) and processed results are passed back
# to the database

# Protection level is calculated separately for each assessment method (area, population, or localities)
# There is also two protection level calculations - one considering protected area effectiveness, 
# and one not considering effectiveness. This allows for the impact of ineffective PA management to be quantified.

# ---- load libraries ---------------------------------
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data
library(dplyr)     # For general data wrangling

# ---- set up reusable variables for current assessment -----------------------------------------------

# This means that the script can be reused for future assessments without needing to change anything other than
# these variables.
var_current_assessment_id <- 2

# ---- establish database connection --------------------------------------------------------------

# Note that this assumes a script called db-connection.R is present in the same folder as this script
# If the database is in a Docker container, it assumes that the container is running
# The database login information is contained in an .Renviron file (not part of the repo)

source("db-connection.R")

# ---- get protection level calculation function --------------------

source("function-calculate-protection-level.R")

# ---- get information about time points for the assessment from the PLAD ------

query_time_points <- paste0("SELECT assessment_id, timepoint_id, timepoint_year FROM speciesdata.refassessmenttimepoints WHERE assessment_id = ", var_current_assessment_id)
assessment_time_points <- dbGetQuery(db, query_time_points)

# --- ANALYSIS STEP 1: Add a species list for each time point in the current assessment to tblplassessment -----------

# This ensures that all species are considered in the assessment - even those that are not in any protected areas 
# These species would not have records in tblspeciesinpa. Summarised data from tblspeciesinpa is then added 
# using UPDATE statements where the data applies.

for(t in assessment_time_points$timepoint_id){

            query_insert_assessment_species_list <- paste0("INSERT INTO speciesdata.tblplassessment(
                                                            assessment_id, species_id, timepoint_id)
                                               SELECT ", var_current_assessment_id, 
                                                      ", species_id, ",
                                                      t, 
                                               " FROM speciesdata.tblspecies s 
                                                WHERE s.current_name = 1")

            dbExecute(db, query_insert_assessment_species_list) #This should insert exactly 900 rows each time
}

# ---- ANALYSIS STEP 2: Get relevant data from PLAD for further processing -----

# Get species targets for current assessment
# The same targets are used for each time point
query_species_targets <- paste0("SELECT species_id, population_status, population_target, area_target, subpopulation_target
                            FROM speciesdata.tbltargets WHERE assessment_id = ", var_current_assessment_id)

species_targets <- dbGetQuery(db, query_species_targets) #This should insert exactly 900 rows

# Get species x pa data for the current assessment
# This should return data for all time points in the current assessment
query_species_pa_data <- paste0("SELECT * FROM speciesdata.tblspeciesinpa
                            WHERE (presence_pa = 1 OR presence_cluster = 1)
                            AND assessment_id = ", var_current_assessment_id)

species_pa_data <- dbGetQuery(db, query_species_pa_data) #This query only pulls data where species confirmed present in PA or PA cluster


# ---- ANALYSIS STEP 3: Summarise PA-species data by assessment method ------------

# First, for all species, calculate number of PAs species is present in
# Count the number of PAs the species is in, not the number of PA sections
pa_summary_per_species <- species_pa_data %>% group_by(species_id, assessment_id, timepoint_id) %>% 
                    summarise(number_pas = n_distinct(pa_id)) %>% ungroup()

# Total habitat area protected per species
habitat_protected_per_species <- species_pa_data %>% tidyr::drop_na(area_habitat_pa) %>% 
                                  group_by(species_id, assessment_id, timepoint_id) %>% 
                                summarise(total_habitat_pas = as.integer(sum(area_habitat_pa))) %>% ungroup()

# Total population protected per species

population_protected_per_species <- species_pa_data %>% tidyr::drop_na(population_pa) %>% 
                                      group_by(species_id, assessment_id, timepoint_id) %>%
                                    summarise(total_population_pas = as.integer(sum(population_pa))) %>% ungroup()

# Total localities protected per species
localities_protected_per_species <- species_pa_data %>% tidyr::drop_na(localities_pa) %>% 
                                        group_by(species_id, assessment_id, timepoint_id) %>%
                                    summarise(total_localities_pas = as.integer(sum(localities_pa))) %>% ungroup()

# Join all the data together
pa_summary_per_species <- pa_summary_per_species %>% 
                          left_join(habitat_protected_per_species, by = c("species_id", "assessment_id", "timepoint_id")) %>% 
                          left_join(population_protected_per_species, by = c("species_id", "assessment_id", "timepoint_id")) %>% 
                          left_join(localities_protected_per_species, by = c("species_id", "assessment_id", "timepoint_id"))

# Pass data to PLAD
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmppa_summary_per_species"), 
                          value = pa_summary_per_species, overwrite = TRUE)

query_update_species_pa_summaries <- "UPDATE speciesdata.tblplassessment as pla
                                        SET number_pas = tmp.number_pas,
                                        total_habitat_pas = tmp.total_habitat_pas,
                                        total_population_pas = tmp.total_population_pas,
                                        total_localities_pas = tmp.total_localities_pas
                                    FROM speciesdata.tmppa_summary_per_species tmp
                                    WHERE pla.species_id = tmp.species_id AND 
                                            pla.assessment_id = tmp.assessment_id AND
                                            pla.timepoint_id = tmp.timepoint_id"

dbExecute(db, query_update_species_pa_summaries)

# Drop temporary table
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmppa_summary_per_species")

# ---- ANALYSIS STEP 4: Calculate protection level not considering effectiveness ------------

protection_level_no_effectiveness <- calculate_protection_level(assessment_time_points, 
                                                                species_targets,
                                                                species_pa_data)

# Pass data to PLAD
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpprotection_level_no_effectiveness"), 
             value = protection_level_no_effectiveness, overwrite = TRUE)

query_update_pl_no_effectiveness <- "UPDATE speciesdata.tblplassessment as pla
                                      SET protectionscore_no_effectiveness = tmp.protectionscore,
                                          protectioncategory_no_effectiveness = CAST(tmp.protectioncategory AS speciesdata.protectionlevelcategories)
                                      FROM speciesdata.tmpprotection_level_no_effectiveness tmp
                                      WHERE pla.species_id = tmp.species_id AND 
                                            pla.assessment_id = tmp.assessment_id AND
                                            pla.timepoint_id = tmp.timepoint_id"
                                            
                                            

dbExecute(db, query_update_pl_no_effectiveness)

# Drop temporary table
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpprotection_level_no_effectiveness")

# ---- ANALYSIS STEP 5: Calculate protection level with effectiveness ------------

# Convert effectivenes code to effectiveness weighting
# and apply to pa habitat, population and localities data
species_pa_data_effectiveness <- species_pa_data %>% 
                                  mutate(pa_effectiveness_weight = case_when(pa_effectiveness == 1 ~ 0.1,
                                                                             pa_effectiveness == 2 ~ 0.5,
                                                                             pa_effectiveness == 3 ~ 1),
                                         area_habitat_pa = if_else(!is.na(area_habitat_pa), area_habitat_pa*pa_effectiveness_weight, NA),
                                         population_pa = if_else(!is.na(population_pa), population_pa*pa_effectiveness_weight, NA),
                                         localities_pa = if_else(!is.na(localities_pa), localities_pa*pa_effectiveness_weight, NA))

# Calculate protection level
protection_level_effectiveness <- calculate_protection_level(assessment_time_points, 
                                                             species_targets,
                                                             species_pa_data_effectiveness)

# Pass data to PLAD
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpprotection_level_effectiveness"), 
             value = protection_level_effectiveness, overwrite = TRUE)

query_update_pl_effectiveness <- "UPDATE speciesdata.tblplassessment as pla
                                      SET protectionscore = tmp.protectionscore,
                                          protectioncategory = CAST(tmp.protectioncategory AS speciesdata.protectionlevelcategories)
                                      FROM speciesdata.tmpprotection_level_effectiveness tmp
                                      WHERE pla.species_id = tmp.species_id AND 
                                            pla.assessment_id = tmp.assessment_id AND
                                            pla.timepoint_id = tmp.timepoint_id"



dbExecute(db, query_update_pl_effectiveness)

# Drop temporary table
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpprotection_level_effectiveness")

# ---- close database connection --------------------
dbDisconnect(db)