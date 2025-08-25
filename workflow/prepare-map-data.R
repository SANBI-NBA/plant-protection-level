# Reviewing and updating input variables for species' suitable habitat maps
# Species that may need map updates are identified
# Checking that all species in need of map updates have their input variables correctly coded
# Preparing input data files for creating suitable habitat maps

# ---- load libraries ---------------------------------
library(dplyr)     # For data wrangling
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data
library(sf)        # For doing spatial calculations
library(purrr)     # For applying functions to lists/tibbles
library(units)     # For working with units
library(stringr)   # For making comma concatenated strings of vegmap raster codes

# ---- establish database connection ------------------

# Note that this assumes a script called db-connection.R is present in the same folder as this script
# If the database is in a Docker container, it assumes that the container is running
# The database login information is contained in an .Renviron file (not part of the repo)

source("db-connection.R")

# ---- set up reusable variables for current assessment ---------

# This means that the script can be reused for future assessments without needing to change anything other than
# these variables. We primarily want to compare occurrence records from the previous assessment to the current assessment
# Significant changes in the data might mean that we need to redo or update a species' suitable habitat map

var_last_assessment_id <- 1 
var_current_assessment_id <- 2

# ---- extract data from database -------------------------------

tblspecieschanges <- dbGetQuery(db, paste0("SELECT * FROM speciesdata.tblspecieschanges 
                                           WHERE new_assessment_id = ", var_current_assessment_id))

# use st_read for the spatial data
previous_occurrence_data <- st_read(db, query = paste0("SELECT * FROM speciesdata.geospeciesoccurrences 
                                                  WHERE ", var_last_assessment_id, " = ANY(assessments)"))

# to get the current data is a bit trickier, because the data obtained through the iNaturalist API call has
# not yet been QCd. We therefore restrict the query to data from the Plant Red List database, which has been QCd.
# We can include data from the Plant Survey App, because it is only able to record species within their known distribution
# ranges. The challenge is that we are reusing records from the previous assessment where we can, which is recorded in
# the assessments array. But for new data we only add the current assessment ID after QC. QC can only be done once we have
# the correct maps for all species. This way of doing things will probably cause problems, but at this stage I cannot yet
# think of anything better.

# WHERE (assessment_id = ", var_current_assessment_id, " AND locality_source <> 'iNaturalist API call') - gets all the new records except iNaturalist API call data
# OR (assessment_id = ", var_last_assessment_id, " AND ", var_current_assessment_id, " = ANY(assessments))")) - gets all the Red List occurrence records used in previous assessment(s) 
# and also in latest occurrence record list from Red List

new_occurrence_data <- st_read(db, query = paste0("SELECT * FROM speciesdata.geospeciesoccurrences 
                                           WHERE (assessment_id = ", var_current_assessment_id, " AND locality_source <> 'iNaturalist API call') 
                                           OR (assessment_id <> ", var_current_assessment_id, " AND ", var_current_assessment_id, " = ANY(assessments))"))

# ---- spatial checks on occurrence records -----------------------------------

# Species with recent taxonomic revisions (tblspecieschanges.taxonomic_revision = 1) need new maps and they are automatically included in 
# the list of maps to update. Species with changes to their Red List assessments may have also had their maps revised - Red List Changes are
# often due to new observations extending the known ranges of species. We need to check which species' range maps changed, because they may need to
# have their suitable habitat parameters updated (e.g. they have been recorded in new vegetation types). We do some crude checks by wrapping a
# convex hull around each species' old and new occurrence records, and calculating the overlap

# First we make a list of species that need their maps checked for changes
species_map_checks <- tblspecieschanges %>% filter(rl_change == 1 & taxonomic_revision == 0)

# Then we filter the occurrence datasets for the occurrence records for these species
previous_occurrence_data_checks <- previous_occurrence_data %>% filter(species_id %in% species_map_checks$previous_species_id)
new_occurrence_data_checks <- new_occurrence_data %>% filter(species_id %in% species_map_checks$new_species_id)

# Geospeciesoccurrences is in EPSG4326 (Geographic) - need to project to do spatial calculations
previous_occurrence_data_checks <- st_transform(previous_occurrence_data_checks, 9221)
new_occurrence_data_checks <- st_transform(new_occurrence_data_checks, 9221)

# Count points per species (for extra information)
counts_previous <- previous_occurrence_data_checks %>% 
                        st_drop_geometry() %>%
                        count(species_id, name = "count_points_previous")

counts_new <- new_occurrence_data_checks %>% 
                        st_drop_geometry() %>%
                        count(species_id, name = "count_points_new")

# Create convex hulls per species and calculate area
hulls_previous <- previous_occurrence_data_checks %>%
                        group_by(species_id) %>%
                        summarise(geom = st_combine(geom) |> st_convex_hull(), .groups = 'drop') %>% 
                        mutate(area_previous = st_area(geom))

hulls_new <- new_occurrence_data_checks %>%
                  group_by(species_id) %>%
                  summarise(geom = st_combine(geom) |> st_convex_hull(), .groups = 'drop') %>% 
                  mutate(area_new = st_area(geom))

# Calculate overlaps between old and new geoms
intersection <- st_intersection(hulls_previous, hulls_new, by_feature = TRUE) %>% 
                filter(species_id == species_id.1) %>% 
                mutate(area_overlap = st_area(geom))

# Join all the data back together
hulls_previous <- hulls_previous %>% select(species_id, area_previous) %>% st_drop_geometry()
previous_data <- left_join(counts_previous, hulls_previous, by = "species_id")

hulls_new <- hulls_new %>% select(species_id, area_new) %>% st_drop_geometry()
new_data <- left_join(counts_new, hulls_new, by = "species_id")

intersection <- intersection %>% select(species_id, area_overlap) %>% st_drop_geometry()

species_map_checks <- left_join(species_map_checks, previous_data, by = c("previous_species_id" = "species_id"))
species_map_checks <- left_join(species_map_checks, new_data, by = c("new_species_id" = "species_id"))

species_map_checks <- left_join(species_map_checks, intersection, by = c("new_species_id" = "species_id"))

# Identify species with map changes
species_map_checks <- species_map_checks %>% mutate(map_change = case_when(area_previous == area_new & area_new == area_overlap ~ "no change",
                                                                           area_previous == area_new & count_points_previous == count_points_new ~ "no change",
                                                                           area_previous < area_new ~ "range extension",
                                                                           area_previous > area_new ~ "range contraction",
                                                                           TRUE ~ "other"),
                                                    perc_overlap = case_when(area_previous == area_new & area_new == area_overlap ~ 100,
                                                                             area_previous == area_new & area_new != area_overlap ~ as.numeric((area_overlap/area_new)*100),
                                                                             count_points_new >= 3 & area_previous < area_new ~ as.numeric((area_overlap/area_new)*100),
                                                                             count_points_previous >= 3 & area_previous > area_new ~ as.numeric((area_overlap/area_previous)*100),
                                                                             TRUE ~ NA),
                                                    update = as.integer(if_else(perc_overlap >= 90,0,NA)),
                                                    area_previous = as.numeric(area_previous),
                                                    area_new = as.numeric(area_new),
                                                    area_overlap = as.numeric(area_overlap))

# Load this output as a temporary table into the database
# Check all the species where update is NA, and set to 0 (map does not need updating) or 1 (map needs updating)
# Also fix suitable habitat variables where necessary and QC points
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpspecies_map_checks"), value = species_map_checks, overwrite = TRUE)

# We have to reset the primary key to make this table editable
dbExecute(db, "ALTER TABLE speciesdata.tmpspecies_map_checks ADD CONSTRAINT tmpspecies_map_checks_pkey PRIMARY KEY (change_id)")
#--------------------------------------------------------------------------------------------------------------------------------
# Check all the species in QGIS
#--------------------------------------------------------------------------------------------------------------------------------

# ---- check and prepare input data for species needing new maps -------------------------------------------------

# Update tblspecieschanges and tblspecies with new map IDs
species_map_checks <- dbGetQuery(db, "SELECT change_id, update FROM speciesdata.tmpspecies_map_checks")

tblspecieschanges <- dbGetQuery(db, paste0("SELECT * FROM speciesdata.tblspecieschanges 
                                           WHERE new_assessment_id = ", var_current_assessment_id))

new_map_ids <- left_join(tblspecieschanges, species_map_checks, by = "change_id")
new_map_ids <- new_map_ids %>% mutate(new_map_id = case_when(taxonomic_revision == 1  ~ paste0("m-", new_species_id, "-", var_current_assessment_id),
                                                              update == 1 ~ paste0("m-", new_species_id, "-", var_current_assessment_id),
                                                              TRUE ~ NA)) %>% 
                              filter(!is.na(new_map_id)) %>% 
                              select(change_id, new_species_id, new_map_id)

# Update data in tblspecieschanges
dbWriteTable(db, name = DBI::Id(schema = "speciesdata", table = "tmpnew_map_ids"), value = new_map_ids, overwrite = TRUE)
dbExecute(db, "UPDATE speciesdata.tblspecieschanges as s
                SET new_map_id = m.new_map_id
                FROM speciesdata.tmpnew_map_ids as m
                WHERE s.change_id = m.change_id") 

# Update data in tblspecies
dbExecute(db, "UPDATE speciesdata.tblspecies as s
                SET map_id = m.new_map_id
                FROM speciesdata.tmpnew_map_ids as m
                WHERE s.species_id = m.new_species_id")

# clean up the temporary tables
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpspecies_map_checks")
dbExecute(db, "DROP TABLE IF EXISTS speciesdata.tmpnew_map_ids")

# Pull out data from database for input csv files

# ---- pointdata.csv --------------------------------------------------------------------------
point_query <- paste0("select C.new_map_id, o.species_id, o.latitude, o.longitude, o.precisionm
    from speciesdata.geospeciesoccurrences o inner join speciesdata.tblspecieschanges c on
    o.species_id = c.new_species_id where c.new_map_id is not null and c.new_assessment_id = ", var_current_assessment_id, " 
    and o.qc = 1")

pointdata <- dbGetQuery(db, point_query)

# check that all the species have QCd points
pointdatachecks <- pointdata %>% group_by(species_id) %>% summarise(count = n())
pointdatachecks <- left_join(new_map_ids, pointdatachecks, by = c("new_species_id" = "species_id"))

# wrangle to match QGIS model input
pointdata <- pointdata %>% select(new_map_id, latitude, longitude, precisionm) %>% 
                            rename(GenSpec = new_map_id, DDS = latitude, DDE = longitude)

# write to csv
write.csv(pointdata, file = "data/pointdata.csv", row.names = FALSE, quote = FALSE)

# ---- altitude (DEM) data ----------------------------------------------------------------------
dem_query <- paste0("select s.map_id, s.species_id, s.minalt, s.maxalt from speciesdata.tblspecies s
                      inner join speciesdata.tblspecieschanges c on s.species_id = c.new_species_id
                      where c.new_map_id is not null and c.new_assessment_id = ", var_current_assessment_id)

demdata <- dbGetQuery(db, dem_query) #This should match the number of records in new_map_ids

# wrangle to match QGIS model input
demdata <- demdata %>% select(map_id, minalt, maxalt) %>% rename(Genspec = map_id)

# write to csv
write.csv(demdata, file = "data/demdata.csv", row.names = FALSE, quote = FALSE)

# ---- landformdata.csv ----------------------------------------------------------------------
landform_query <- paste0("select c.new_map_id, s.species_id, s.lf1, s.lf2, s.lf3, s.lf4, s.lf5, s.lf6
                        from speciesdata.tblspecieslandforms s inner join speciesdata.tblspecieschanges c
                        on s.species_id = c.new_species_id where c.new_map_id is not null and c.new_assessment_id = ",
                         var_current_assessment_id)

landformdata <- dbGetQuery(db, landform_query) #This should match the number of records in new_map_ids

# wrangle to match QGIS model input
landformdata <- landformdata %>% select(new_map_id, lf1, lf2, lf3, lf4, lf5, lf6) %>% rename(Genspec = new_map_id,
                                                                                             "1" = lf1, "2" = lf2, "3" = lf3,
                                                                                             "4" = lf4, "5" = lf5, "6" = lf6)

# write to csv
write.csv(landformdata, file = "data/landformdata.csv", row.names = FALSE, quote = FALSE)

# ---- vegdata.csv ----------------------------------------------------------------------
veg_query <- paste0("select c.new_map_id, s.species_id, v.rastercode from speciesdata.tblspeciesvegetation s
                    inner join speciesdata.tblspecieschanges c on s.species_id = c.new_species_id
                    inner join speciesdata.refvegcodes v on s.mapcode = v.mapcode where c.new_map_id is not null
                    and c.new_assessment_id = ", var_current_assessment_id, " order by c.new_map_id, v.rastercode")

vegdata <- dbGetQuery(db, veg_query)

# check that all the species have vegtypes coded
vegdatachecks <- vegdata %>% group_by(species_id) %>% summarise(count = n())

# wrangle to match QGIS model input
vegdata <- vegdata %>% group_by(new_map_id) %>% summarise(vegcodes = str_c(rastercode, collapse = ","), .groups = "drop") %>% 
                        rename(genspec = new_map_id)

# write to csv
write.csv(vegdata, file = "data/vegdata.csv", row.names = FALSE)

# ---- close database connection --------------------
dbDisconnect(db)
