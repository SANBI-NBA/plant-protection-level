# Script to apply species-specific effectiveness ratings to species x pa combinations
# This script pulls data from the PLAD, wrangles it, and then sends updated data back

# ---- load libraries ---------------------------------
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data
library(dplyr)     # For general data wrangling

# ---- establish database connection ------------------

# Note that this assumes a script called db-connection.R is present in the same folder as this script
# If the database is in a Docker container, it assumes that the container is running
# The database login information is contained in an .Renviron file (not part of the repo)

source("db-connection.R")

# ---- set up reusable variables for current assessment ---------

# This means that the script can be reused for future assessments without needing to change anything other than
# these variables.

var_current_assessment_id <- 2

# ---- get relevant data from the PLAD -------

# species list
query_species_list <- "SELECT species_id, \"family\", resilient, growth_form, collected FROM speciesdata.tblspecies WHERE current_name = 1"
species_list <- dbGetQuery(db, query_species_list)

# PA general effectiveness
query_pa_effectiveness <- paste0("SELECT assessment_id, timepoint_id, pa_id, pa_section_id, pa_cluster_id, general_effectiveness, 
                                  livestock, poaching, elephants
                            FROM speciesdata.tblpaeffectiveness
                            WHERE assessment_id = ", var_current_assessment_id)

pa_effectiveness <- dbGetQuery(db, query_pa_effectiveness)

# Species x PA data
# This extracts all species x pa combinations, regardless of whether species is confirmed present or not

query_species_in_pa <- paste0("SELECT assessment_id, timepoint_id, species_id, pa_id, pa_section_id, pa_cluster_id, pa_effectiveness
                              FROM speciesdata.tblspeciesinpa
                              WHERE assessment_id = ", var_current_assessment_id)

species_in_pa <- dbGetQuery(db, query_species_in_pa)

# ---- group species by different sensitivities to pressures --------

# Species sensitive to grazing
# These include non-resilient species with shrub or herb growth forms, or in the grass (Poaceae) family

species_list <- species_list %>% mutate(grazing_sensitive = as.integer(if_else(resilient == 0 & (growth_form == "herb" |
                                                                                                 growth_form == "shrub" |
                                                                                                   family == "Poaceae"), 1, 0)))

# Species sensitive to poaching
# Any species where collected = 1 (including those resilient to disturbance)

# Then there are some reserves where large elephant populations are causing a decline in tree species
species_list <- species_list %>% mutate(elephant_sensitive = as.integer(if_else(growth_form == "tree", 1, 0)))

# Keep only the relevant data for joining to other tables
species_list_join <- species_list %>% select(species_id, resilient, collected, grazing_sensitive, elephant_sensitive)

# ---- join data together -----------------------

species_in_pa <- inner_join(species_in_pa, species_list_join, by = "species_id")
species_in_pa <- inner_join(species_in_pa, pa_effectiveness, 
                            by = c("assessment_id", "timepoint_id", "pa_id", "pa_section_id", "pa_cluster_id"))

# ---- apply effectiveness rules ---------

# Rule 1: Resilient species gets effectiveness = 3 (good) regardless of PA effectiveness, other species get general PA effectiveness
species_in_pa <- species_in_pa %>% mutate(pa_effectiveness = if_else(resilient == 1, 3, general_effectiveness))

# Species with specific sensitivities gets their effectiveness adjusted, depending on the pressure inside the PA

# Rule 2: Species with grazing sensitivity gets their effectiveness adjusted by -1 (unless PA effectiveness already 1)
#         where livestock has been noted as present inside the PA

species_in_pa <- species_in_pa %>% mutate(pa_effectiveness = if_else(grazing_sensitive == 1 &
                                                                       livestock == 1 &
                                                                       general_effectiveness > 1, pa_effectiveness - 1, pa_effectiveness))

# Rule 3: Species targeted by poachers gets their effectiveness adjusted by -1 (unless PA effectiveness already 1)
#         where poaching has been noted as happening inside the PA

species_in_pa <- species_in_pa %>% mutate(pa_effectiveness = if_else(collected == 1 & poaching == 1 & general_effectiveness > 1,
                                                                     pa_effectiveness - 1, pa_effectiveness))

# Rule 4: Trees in PAs with elephant overpopulation gets their effectiveness adjusted by -1 (unless PA effectiveness already 1)

species_in_pa <- species_in_pa %>% mutate(pa_effectiveness = if_else(elephant_sensitive == 1 & elephants == 1 & general_effectiveness > 1,
                                                                     pa_effectiveness - 1, pa_effectiveness))

# It happens that some species vulnerable to multiple pressures end up with scores <1 after application of the rules
# Need to reset effectiveness for these (cannot be <1)

species_in_pa <- species_in_pa %>% mutate(pa_effectiveness = if_else(pa_effectiveness <1, 1, pa_effectiveness))

# ---- Pass data back to the db -------

# first fix pa_effectiveness back to integer (R changes it to numeric when applying mutate)
species_in_pa <- species_in_pa %>% mutate(pa_effectiveness = as.integer(pa_effectiveness))

dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpspecies_in_pa_effectiveness"), 
             value = species_in_pa, overwrite = TRUE)

# Update the effectiveness values in tblspeciesinpa from the imported data

query_update_effectiveness = "UPDATE speciesdata.tblspeciesinpa AS sp 
                              SET pa_effectiveness = tmp.pa_effectiveness
                              FROM speciesdata.tmpspecies_in_pa_effectiveness AS tmp
                              WHERE sp.species_id = tmp.species_id AND 
                                    sp.pa_id = tmp.pa_id AND 
                                    sp.pa_section_id = tmp.pa_section_id AND 
                                    sp.pa_cluster_id = tmp.pa_cluster_id AND 
                                    sp.assessment_id = tmp.assessment_id AND 
                                    sp.timepoint_id = tmp.timepoint_id"

dbExecute(db, query_update_effectiveness)

# Remove temporary table
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpspecies_in_pa_effectiveness")

# ---- close database connection --------------------
dbDisconnect(db)



