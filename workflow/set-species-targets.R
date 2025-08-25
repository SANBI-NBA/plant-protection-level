# Workflow for setting species conservation targets for the current assessment
# Based on latest available input data. The rationales behind targets are explained in xxxx
# This script relies on the outputs of several earlier processes in the data preparation process:
#     - tmpnewrldata (table in PLAD) - produced in process-rl-data.R
#     - occurrence records have been updated and QCd
#     - maps have been updated aligned with latest Red List assessments
#     - output of produce-suitable-habitat-maps.R - suitable-habitat-model-output-summary.csv is in the workflow folder

# ---- load libraries ---------------------------------
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data
library(dplyr)     # For general data wrangling

# ---- set up reusable variables for current assessment -----------------------------------------------

# This means that the script can be reused for future assessments without needing to change anything other than
# these variables.
var_current_assessment_id <- 2
var_previous_assessment_id <- 1

# ---- establish database connection --------------------------------------------------------------

# Note that this assumes a script called db-connection.R is present in the same folder as this script
# If the database is in a Docker container, it assumes that the container is running
# The database login information is contained in an .Renviron file (not part of the repo)

source("db-connection.R")

# ---- update PLAD with new RL data -----------------------------------------------------------

# At this stage new Red List data has been processed as part of the RL occurrences import/update workflow
# It is stored in a temporary table in the database (tmpnewrldata)
# But not yet formally integrated into the PLAD
# Latest species RL status is updated in tblspecies
# RL status changes from previous assessments have already been recorded in tblspecieschanges

query_rl_update <- "UPDATE speciesdata.tblspecies as s
                    SET rl_status = n.rl_status, rl_criteria = n.rl_criteria, rl_date = n.rl_date
                    FROM speciesdata.tmpnewrldata as n
                    WHERE s.species_id = n.species_id"

dbExecute(db, query_rl_update)

# ---- create new target list for current assessment ------------------------------------------

query_new_target_list <- paste0("INSERT INTO speciesdata.tbltargets(
                                  assessment_id, species_id) 
                                SELECT ", var_current_assessment_id, ", species_id
                                FROM speciesdata.tblspecies WHERE current_name = 1")

dbExecute(db, query_new_target_list)

# ---- Populate with population data from Red List assessments -------------------------------

query_add_rl_population_data <- paste0("UPDATE speciesdata.tbltargets as t
                                       SET population_description = n.population_description, min_population_size = n.min_population_size, 
                                       max_population_size = n.max_population_size
                                       FROM speciesdata.tmpnewrldata as n
                                       WHERE t.species_id = n.species_id AND t.assessment_id = ", var_current_assessment_id)

dbExecute(db, query_add_rl_population_data)

# ---- review and update abundance categories for species ----------------------------------

# First check that all species have an abundance record in tblspeciesabundance

query_missing_abundance_records <- paste0("SELECT s.species_id, s.taxon, t.population_description
                                      FROM speciesdata.tblspecies s INNER JOIN speciesdata.tbltargets t on s.species_id = t.species_id
                                      LEFT JOIN speciesdata.tblspeciesabundance a ON s.species_id = a.species_id
                                      WHERE a.species_id IS NULL AND t.assessment_id = ", var_current_assessment_id)

missing_abundance <- dbGetQuery(db, query_missing_abundance_records)
# This query should return species recorded under taxonomic changes in tblspecieschanges for the current assessment
# The dataframe created here (missing_abundance) can be used to see if Red List population descriptions provide any useful information regarding abundance
# Otherwise literature/herbarium specimens need to be consulted

# Insert the missing species into tblspeciesabundance (without abundance categories assigned)
# Find the records for these species in tblspeciesabundance and assign appropriate abundance categories

query_insert_missing_abundance <- paste0("INSERT INTO speciesdata.tblspeciesabundance (species_id, assessments)
                                            SELECT s.species_id, array[", var_current_assessment_id, "]
                                          FROM speciesdata.tblspecies s LEFT JOIN speciesdata.tblspeciesabundance a ON s.species_id = a.species_id
                                          WHERE a.species_id IS NULL")

dbExecute(db, query_insert_missing_abundance)

# Review abundance categories from previous assessment based on Red List population descriptions
query_abundance_revision <- paste0("SELECT a.species_id, s.taxon, t.population_description, a.abundance, r.abundance_category
              FROM speciesdata.tblspeciesabundance a INNER JOIN speciesdata.tblspecies s ON a.species_id = s.species_id
              INNER JOIN speciesdata.tbltargets t ON a.species_id = t.species_id INNER JOIN speciesdata.refabundance r ON a.abundance = r.abundance_id
              WHERE ", var_previous_assessment_id, " = ANY(a.assessments) AND t.assessment_id = ", var_current_assessment_id, " 
              AND t.population_description <> '' AND s.current_name = 1")

abundance_review <- dbGetQuery(db, query_abundance_revision)
abundance_review <- abundance_review %>% mutate(new_abundance = 0)

write.csv(abundance_review, file = "abundance_review.csv", row.names = FALSE)

# This file gets checked manually - read population notes and decide whether assigned abundance category is appropriate.
# Often species that were previously poorly known have new information after new field data, and this is reflected in population notes
# associated with Red List assessments. The column new_abundance is modified where new information indicates that abundance estimates
# need to be updated. I found it easier to keep track of which ones I checked by deleting the automated 0s. Therefore the following
# code drops NAs to find the species with updated abundance.

reviewed_abundance <- read.csv("abundance_review.csv", header = TRUE)
reviewed_abundance <- reviewed_abundance %>% tidyr::drop_na(new_abundance)

# These records are inserted into the abundance table as new records
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpreviewed_abundance"), value = reviewed_abundance, overwrite = TRUE)

# Write a note to explain the source of the new abundance data
txt_abundance_source <- "'NBA 2025 Red List index population notes'"

query_insert_new_abundance <- paste0("INSERT INTO speciesdata.tblspeciesabundance (species_id, abundance, abundance_source, assessments)
                                          SELECT species_id, new_abundance, ", txt_abundance_source, ", array[", var_current_assessment_id, "]
                                          FROM speciesdata.tmpreviewed_abundance")

dbExecute(db, query_insert_new_abundance)

# clean up the temporary table
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpreviewed_abundance")

# For the rest of the species, re-use abundance data from previous assessment
query_update_abundance_to_current_assessment <- paste0(
"UPDATE speciesdata.tblspeciesabundance tsa SET assessments = tsa.assessments || ", var_current_assessment_id, "
WHERE tsa.assessments @> ARRAY[", var_previous_assessment_id, "] AND tsa.species_id IN (SELECT species_id FROM speciesdata.tblspecies ts
WHERE ts.current_name = 1
  AND ts.species_id NOT IN (
    SELECT species_id
    FROM speciesdata.tblspeciesabundance
    WHERE assessments @> ARRAY[", var_current_assessment_id, "]
  )
)")

dbExecute(db, query_update_abundance_to_current_assessment)

# Add new abundance data to tblspeciestargets

query_add_abundance_to_targets <- paste0("UPDATE speciesdata.tbltargets as t 
                                         SET abundance = a.abundance
                                         FROM speciesdata.tblspeciesabundance as a 
                                         WHERE t.species_id = a.species_id AND 
                                         t.assessment_id = ", var_current_assessment_id, "
                                         AND ", var_current_assessment_id, " = ANY(a.assessments)")


dbExecute(db, query_add_abundance_to_targets)

# ---- set density data for new targets ----------------------------------

# Density data comes from two sources: a species' abundance category, or tblspeciesdensity, which records species-specific
# density from field surveys. Both need to be added to tbltargets for the current assessment

# Check and apply existing density data to the current assessment (basically the same query as for abundance)

query_update_density_to_current_assessment <- paste0(
  "UPDATE speciesdata.tblspeciesdensity tsd SET assessments = tsd.assessments || ", var_current_assessment_id, "
  WHERE tsd.assessments @> ARRAY[", var_previous_assessment_id, "] AND tsd.species_id IN (SELECT species_id FROM speciesdata.tblspecies ts
  WHERE ts.current_name = 1
  AND ts.species_id NOT IN (
    SELECT species_id
    FROM speciesdata.tblspeciesdensity
    WHERE assessments @> ARRAY[", var_current_assessment_id, "]
  )
)")

dbExecute(db, query_update_density_to_current_assessment)

# add current assessment's density data to tbltargets

# First abundance-linked density for all species

query_add_abundance_density_to_targets <- paste0("UPDATE speciesdata.tbltargets as t
                                                 SET density = a.density,
                                                 density_source = 'abundance'
                                                 FROM speciesdata.refabundance as a
                                                 WHERE t.abundance = a.abundance_id AND 
                                                 t.assessment_id = ", var_current_assessment_id)

dbExecute(db, query_add_abundance_density_to_targets)

# Then species-specific density

query_add_species_density_to_targets <- paste0("UPDATE speciesdata.tbltargets as t 
                                               SET density = s.density,
                                               density_source = 'speciesdensity'
                                               FROM speciesdata.tblspeciesdensity as s
                                               WHERE t.species_id = s.species_id AND
                                               t.assessment_id = ", var_current_assessment_id, " AND ",
                                               var_current_assessment_id, " = ANY(s.assessments)")

dbExecute(db, query_add_species_density_to_targets)


# ---- add total habitat estimates ----------------------------------

# For species with new maps, this data is retrieved from the mapping outputs
# For other species, the area from the previous assessment is reused

map_data <- read.csv("suitable-habitat-model-output-summary.csv", header = TRUE)
species_ids <- dbGetQuery(db, "SELECT species_id, map_id FROM speciesdata.tblspecies")
map_data <- left_join(map_data, species_ids, by = "map_id")

# Pass map data to database
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpmap_data"), value = map_data, overwrite = TRUE)

# Add map_data's area_ha to the current assessment

query_add_new_area_ha <- paste0("UPDATE speciesdata.tbltargets as t
                                  SET totalhabitatha = n.area_ha
                                  FROM speciesdata.tmpmap_data as n
                                  WHERE t.species_id = n.species_id AND
                                  t.assessment_id = ", var_current_assessment_id)

dbExecute(db, query_add_new_area_ha)

# clean up the temporary table
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpmap_data")

# Fill in habitat area from previous assessment to current assessment

query_update_area_ha <- paste0("UPDATE speciesdata.tbltargets as t
                                  SET totalhabitatha = sq.totalhabitatha
                                FROM (
                                  SELECT species_id, totalhabitatha
                                  FROM speciesdata.tbltargets
                                  WHERE assessment_id = ", var_previous_assessment_id, ") AS sq
                                WHERE t.species_id = sq.species_id AND
                                  t.totalhabitatha IS NULL AND
                                  t.assessment_id = ", var_current_assessment_id)

dbExecute(db, query_update_area_ha)

# ---- estimate population size from density and habitat for species without population data -----------------------

query_estimate_population_size <- paste0("UPDATE speciesdata.tbltargets 
                                         SET estimated_population_size = ROUND(density*totalhabitatha)
                                         WHERE max_population_size IS NULL AND
                                         assessment_id = ", var_current_assessment_id)

dbExecute(db, query_estimate_population_size)

# ---- calculate number of occurrences ------------------------------------------------------------------

query_update_occurrences <- paste0("UPDATE speciesdata.tbltargets as t
                                      SET number_occurrences = sq.number_occurrences
                                      FROM (
                                        SELECT 
                                            species_id, COUNT(occurrence_id) as number_occurrences
                                        FROM speciesdata.geospeciesoccurrences 
                                        WHERE ", var_current_assessment_id, " = ANY(assessments) AND qc = 1
                                        GROUP BY species_id) AS sq
                                      WHERE t.species_id = sq.species_id AND
                                        t.assessment_id = ", var_current_assessment_id)

dbExecute(db, query_update_occurrences)

# ---- derive densities from Red List data ---------------------------------------------------

# Some species are recorded in Red List assessments to have populations <10 000, but they do not meet
# Red List criteria C or D. This means that although the total population size is known, there may not be sufficient
# locality level population data to use the population method to measure progress towards targets.
# For these species, the area method is used to assess protection level, but density is scaled to the extent of 
# suitable habitat, so that if all suitable habitat is protected, it should add up to the Red List population size
# This method does not quite work for threatened species, which may have lost a lot of habitat (and therefore density
# would be higher if lost habitat was excluded), but it is too complicated at this stage to calculate remaining habitat.
# In any case, if the species is threatened, even if all habitat is protected it cannot be Well Protected, because
# the area target would be larger than the total available habitat, because the target is set at the area equivalent of
# 10 000 individuals.

query_set_density_from_red_list_population <- paste0("UPDATE speciesdata.tbltargets as t
                                                SET density = t.max_population_size/totalhabitatha,
                                                density_source = 'red list population'
                                              FROM (
                                                SELECT t.species_id
                                                FROM speciesdata.tbltargets t
                                                INNER JOIN speciesdata.tblspecies s ON t.species_id = s.species_id
                                                WHERE t.assessment_id = ", var_current_assessment_id, " 
                                                AND ((s.rl_status = 'LC' AND t.max_population_size >1000 and t.max_population_size <=10000) 
                                                    OR (s.rl_status <> 'LC' AND t.max_population_size <=10000))
                                              ) AS sq    
                                              WHERE t.species_id = sq.species_id AND
                                              t.assessment_id = ", var_current_assessment_id)

dbExecute(db, query_set_density_from_red_list_population)

# ---- determine population status categories --------------------------------------------------------------

# There are seven categories of target and method combinations (see table refpopulationstatus), 
# and making sure each of the 900 species ends up in the right category is very complicated.
# The aim is to set very precise criteria for each category, so that species with internal inconsistencies
# in their Red List status, criteria, and population data drop out/are not assigned to categories, so that at
# the end of the process they can be individually checked with the Threatened Species Unit.
# This is still a work in progress, because not all possible combinations of data have necessarily been considered
# as they were not present in the data at the time this code was written. Near Threatened species in particular can
# be quite tricky to deal with, but there are not many species in the RLI sample with this status.

# First we extract the main variables important for determining target category:
query_target_data <- paste0("SELECT t.assessment_id, t.species_id, s.taxon, s.rl_status, s.rl_criteria,
        t.population_description, t.density, t.totalhabitatha, t.estimated_population_size,
        t.min_population_size, t.max_population_size, t.number_occurrences
        FROM speciesdata.tbltargets t INNER JOIN speciesdata.tblspecies s ON t.species_id = s.species_id
        WHERE t.assessment_id = ", var_current_assessment_id)

target_data <- dbGetQuery(db, query_target_data)

# Some additional data from occurrences are needed to help identify poorly known species, and species where the
# population assessment method can be applied

query_best_precision <- paste0("SELECT species_id, MIN(precisionm) AS best_precision
                               FROM speciesdata.geospeciesoccurrences WHERE qc = 1
                               AND ", var_current_assessment_id, " = ANY(assessments)
                               GROUP BY species_id")

best_precision <- dbGetQuery(db, query_best_precision)

target_data <- left_join(target_data, best_precision, by = "species_id")

query_locality_counts <- paste0("SELECT species_id, COUNT(occurrence_id) AS localities_with_counts
                                FROM speciesdata.geospeciesoccurrences WHERE qc = 1
                                AND ", var_current_assessment_id, " = ANY(assessments)
                                AND (min_count IS NOT NULL OR max_count IS NOT NULL)
                                GROUP BY species_id")

locality_counts <- dbGetQuery(db, query_locality_counts)

target_data <- left_join(target_data, locality_counts, by = "species_id")

# Also replace NAs in Red List criteria with "", otherwise it causes a lot of issues with filtering
target_data <- target_data %>% mutate(rl_criteria = tidyr::replace_na(rl_criteria, ""))


# Systematically select species that belong to each category from the data

# Category 7: Poorly known species
# This category is the hardest to pull out (with potential overlaps with other categories)
# Therefore it is done first

# Species included in this category:
#   - species without suitable habitat maps (totalhabitatha is NA)
#   - species without population data (min population size, max population size, and estimated population size all NA)
#   - species with all their occurrence records with very imprecise localities (precisionm > 5000)
#   - CR PE, Rare, Critically Rare, DD, LC species known from fewer than 5 records

cat_7 <- target_data %>% 
                  mutate(population_status = case_when(is.na(totalhabitatha) ~ 7,
                                               is.na(min_population_size) & is.na(max_population_size) & is.na(estimated_population_size) ~ 7,
                                               best_precision > 5000 ~ 7,
                                               number_occurrences <5 & rl_criteria == ""  ~ 7,
                                               TRUE ~ NA)) %>% 
                  filter(population_status == 7)

target_data <- anti_join(target_data, cat_7, by = "species_id")

# Category 4: Species with known population size <1000 but no evidence of recent or ongoing decline 

cat_4 <- target_data %>% filter(rl_criteria == "D" | rl_criteria == "D1" | rl_criteria == "D1+2") %>% 
                         filter(max_population_size <= 1000) %>% 
                         tidyr::drop_na(localities_with_counts) %>% 
                         mutate(population_status = 4)

cats_assigned <- rbind(cat_7, cat_4)
target_data <- anti_join(target_data, cats_assigned, by = "species_id")

# Category 8: Ad hoc category for category 4 species that do not have localities with counts

# For these species, the target is the area equivalent of 1000 individuals, which cannot be met
# for EN and CR species. In the 2025 assessment there were two species meeting this condition

cat_8 <- target_data %>% filter(rl_criteria == "D" | rl_criteria == "D1" | rl_criteria == "D1+2") %>% 
                          filter(max_population_size <= 1000) %>% 
                          filter(is.na(localities_with_counts)) %>% 
                          mutate(population_status = 8)

cats_assigned <- rbind(cats_assigned, cat_8)
target_data <- anti_join(target_data, cats_assigned, by = "species_id")

# Category 3: Species with known population size <10 000, well-known population structure, and evidence of past or ongoing decline

cat_3 <- target_data %>% filter(grepl("C1", rl_criteria) | grepl("C2", rl_criteria) | endsWith(rl_criteria, "; D")) %>% 
                         filter(max_population_size <= 10000) %>% 
                         tidyr::drop_na(localities_with_counts) %>% 
                         mutate(population_status = 3)

cats_assigned <- rbind(cats_assigned, cat_3)
target_data <- anti_join(target_data, cats_assigned, by = "species_id")

# Category 2: Species with known population size <10 000 and evidence of past or ongoing decline, but not meeting criteria C or D

cat_2 <- target_data %>% filter(max_population_size <= 10000) %>% 
                         mutate(population_status = case_when(startsWith(rl_criteria, "A") | startsWith(rl_criteria, "B") ~ 2,
                                                              rl_status == "CR PE" ~ 2,
                                                              TRUE ~ NA)) %>% 
                        tidyr::drop_na(population_status)

cats_assigned <- rbind(cats_assigned, cat_2)
target_data <- anti_join(target_data, cats_assigned, by = "species_id")  

# Category 6: Declining species with narrow distributions that are suspected to have populations <10 0000

cat_6 <- target_data %>% filter(estimated_population_size <= 10000) %>% 
                         mutate(population_status = case_when(startsWith(rl_criteria, "A") | startsWith(rl_criteria, "B") ~ 6,
                                       rl_status == "CR PE" ~ 6,
                                       TRUE ~ NA)) %>% 
                         tidyr::drop_na(population_status)

cats_assigned <- rbind(cats_assigned, cat_6)
target_data <- anti_join(target_data, cats_assigned, by = "species_id")

# Category 5: Naturally rare or range restricted species with known or suspected small populations, but no evidence of recent or ongoing decline

cat_5 <- target_data %>% filter(rl_criteria == "" | rl_criteria == "D2") %>% 
                         filter(rl_status != "CR PE") %>% 
                         mutate(population_status = case_when(max_population_size > 1000 & max_population_size <= 10000 ~ 5,
                                                              estimated_population_size <= 10000 ~ 5,
                                                              TRUE ~ NA)) %>% 
                         tidyr::drop_na(population_status)

cats_assigned <- rbind(cats_assigned, cat_5)
target_data <- anti_join(target_data, cats_assigned, by = "species_id")

# Category 1: Well-known species with population size known or suspected to be >10 000

cat_1 <- target_data %>% filter(!stringr::str_detect(rl_criteria, "C1")) %>% 
                         filter(!stringr::str_detect(rl_criteria, "C2")) %>% 
                         filter(!endsWith(rl_criteria, "; D")) %>% 
                         mutate(population_status = case_when(estimated_population_size > 10000 ~ 1,
                                                              min_population_size >= 10000 ~ 1,
                                                              max_population_size > 10000 ~ 1,
                                                              TRUE ~ NA)) %>% 
                         tidyr::drop_na(population_status)

cats_assigned <- rbind(cats_assigned, cat_1)
unassigned_categories <- anti_join(target_data, cats_assigned, by = "species_id")

# Review the species in unassigned_categories
# They are either due to cases not considered in the category criteria above (in which case adjust the code as needed)
# Or they are species with internal inconsistencies in their Red List data - in which case, check with Red List team
# what is the correct data. Make corrections in the PLAD as needed, then rerun the code for this section until no species remain
# in unassigned categories

# ---- set targets according to population status categories -------------------------------------------

species_targets <- cats_assigned %>% mutate(population_target = as.integer(case_when(population_status == 4 ~ 1000,
                                                                                     population_status ==3 ~ 10000,
                                                                                     TRUE ~ NA)),
                                            area_target = as.integer(case_when(population_status == 1 | population_status == 2 ~ round(10000/density),
                                                                               population_status == 5 | population_status == 6 ~ totalhabitatha,
                                                                               population_status == 8 ~ round(1000/density),
                                                                               TRUE ~ NA)),
                                            subpopulation_target = as.integer(if_else(population_status == 7, 10, NA)),
                                            target_percentage = as.integer(case_when(population_status == 1 & (area_target/totalhabitatha)*100 <1 ~ 1,
                                                                                     population_status == 1 & (area_target/totalhabitatha)*100 >= 1 ~ round((area_target/totalhabitatha)*100),
                                                                                     TRUE ~ 100)))

species_targets <- species_targets %>% mutate(population_status = as.integer(population_status)) %>% 
                        select(assessment_id, species_id, population_status, population_target,
                                              area_target, subpopulation_target, target_percentage)

# ---- update PLAD with target data ---------------------------------------

dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpspecies_targets"), value = species_targets, overwrite = TRUE)

dbExecute(db, "UPDATE speciesdata.tbltargets as t
                SET population_status = n.population_status,
                    population_target = n.population_target,
                    area_target = n.area_target,
                    subpopulation_target = n.subpopulation_target,
                    target_percentage = n.target_percentage,
                    target_source = 'population status'
                FROM speciesdata.tmpspecies_targets as n
                WHERE t.assessment_id = n.assessment_id AND t.species_id = n.species_id")

# Drop remaining temporary tables
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpspecies_targets")
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpnewrldata")

# ---- close database connection --------------------
dbDisconnect(db)

