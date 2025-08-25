library(dplyr)
library(tidyr)

# This function takes input data on targets, species in protected areas, and assessment time points
# It returns a dataframe with protection scores and protection categories for each species 
# for each provided time point

# The function assumes that the following columns are present in the input data:

# timepoints:
#  - assessment_id
#  - timepoint_id
#  - timepoint_year

# targets:
#  - species_id
#  - area_target
#  - population_target
#  - subpopulation_target

# pa_data:
#  - species_id
#  - assessment_id
#  - timepoint_id
#  - area_habitat_pa
#  - population_pa
#  - localities_pa

calculate_protection_level <- function(timepoints, targets, pa_data){
  
          # Create a target list containing an entry for each time point
          # This is necessary for species not recorded in any PAs, that they are not lost from the results
          assessments <- cross_join(timepoints, targets)
          
          # Summarise pa_data by species for each target type
          habitat_summary <- pa_data %>% drop_na(area_habitat_pa) %>% 
                                         group_by(assessment_id, timepoint_id, species_id) %>% 
                                         summarise(total_habitat_pas = sum(area_habitat_pa)) %>% 
                                         ungroup()
          
          population_summary <- pa_data %>% drop_na(population_pa) %>%
                                            group_by(assessment_id, timepoint_id, species_id) %>%
                                            summarise(total_population_pas = sum(population_pa)) %>%
                                            ungroup()
          
          locality_summary <- pa_data %>% drop_na(localities_pa) %>% 
                                          group_by(assessment_id, timepoint_id, species_id) %>%
                                          summarise(total_localities_pas = sum(localities_pa)) %>%
                                          ungroup()
          
          # Join all the data together
          assessments <- assessments %>%
                              left_join(habitat_summary, by = c("species_id", "assessment_id", "timepoint_id")) %>%
                              left_join(population_summary, by = c("species_id", "assessment_id", "timepoint_id")) %>% 
                              left_join(locality_summary, by = c("species_id", "assessment_id", "timepoint_id"))
          
          # Calculate protectionscore and protectioncategory
          assessments <- assessments %>%
                    mutate(protectionscore = case_when(
                                              !is.na(total_habitat_pas) ~ (total_habitat_pas/area_target)*100,
                                              !is.na(total_population_pas) ~ (total_population_pas/population_target)*100,
                                              !is.na(total_localities_pas) ~ (total_localities_pas/subpopulation_target)*100,
                                             TRUE ~ 0),
                           protectioncategory = case_when(
                                                  protectionscore<5 ~ "Not Protected",
                                                  protectionscore>= 5 & protectionscore <50 ~ "Poorly Protected",
                                                  protectionscore>= 50 & protectionscore <100 ~ "Moderately Protected",
                                                  protectionscore>= 100 ~ "Well Protected",
                                                TRUE ~ NA))
          
          # Return the output
          return(assessments)
}

