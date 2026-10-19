# ---- load libraries ---------------------------------
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data
library(dplyr)     # For general data wrangling
library(tidyr)     # For data pivoting
library(ggplot2)   # For styling plot
library(ggalluvial)# Alluvial plot package

# ---- establish database connection --------------------------------------------------------------
source("../workflow/db-connection.R")

# ---- set up reusable variables for current assessment ---------

# This means that the script can be reused for future assessments without needing to change anything other than
# these variables.

var_current_assessment_id <- 2

# ---- Query protection level categories from database ----

qry_pl_data <- paste0("SELECT a.species_id, a.protectioncategory, t.timepoint_year
                          FROM speciesdata.tblplassessment a
                          INNER JOIN speciesdata.refassessmenttimepoints t ON
                          a.timepoint_id = t.timepoint_id
                          WHERE a.assessment_id = ", var_current_assessment_id)

pl_data <- dbGetQuery(db, qry_pl_data)

# ---- Wrangle data to ggalluvial long format ----

pl_cat_levels <- c("Not Protected",
                   "Poorly Protected",
                   "Moderately Protected",
                   "Well Protected")

pl_cats <- as.data.frame(pl_cat_levels)

pl_cats <- cross_join(pl_cats, pl_cats)
pl_cats <- pl_cats %>% rename(year1 = pl_cat_levels.x, year2 = pl_cat_levels.y)
pl_cats$group <- as.integer(row.names(pl_cats))

pl_data <- pl_data %>% pivot_wider(id_cols = species_id, 
                                   names_from = timepoint_year,
                                   values_from = protectioncategory)

pl_data <- pl_data %>% group_by(`2017`, `2024`) %>% summarise(freq = n()) %>% ungroup()
pl_data <- pl_data %>% mutate(percentage = (freq/900)*100,
                              `2017` = as.character(`2017`),
                              `2024` = as.character(`2024`))

pl_data <- left_join(pl_cats, pl_data, by = c("year1" = "2017", "year2" = "2024"))
pl_data <- pl_data %>% replace(is.na(.), 0)

pl_data$year1 <- factor(pl_data$year1, levels = pl_cat_levels)
pl_data$year2 <- factor(pl_data$year2, levels = pl_cat_levels)

pl_data <- pl_data %>% pivot_longer(cols = c("year1", "year2"),
                                    names_to = "year",
                                    values_to = "plcategory")


pl_strata <- pl_data %>% group_by(year, plcategory) %>% summarise(freq = sum(freq), percentage = sum(percentage))

# ---- make the plot -----

ggplot(pl_data, aes(x = year, stratum = plcategory, alluvium = group, y = percentage)) +
  geom_flow(aes(fill = plcategory), curve_type = "sine") +
  geom_stratum(aes(fill = plcategory), color = NA) +
  geom_text(data = pl_strata, aes(x = year, y = percentage, stratum = plcategory, label = freq), stat = "stratum", inherit.aes = FALSE) +
  scale_fill_manual(values = nbaR::NBA_colours) +
  scale_x_discrete(breaks = c("year1", "year2"), labels = c("2017", "2024"), expand = c(0.05, 0.5)) +
  xlab(NULL) + ylab("Percentage of taxa") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12))