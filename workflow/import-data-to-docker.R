# Basic/generic script for passing data to Docker PostgreSQL database
# It can be used to import any data into a staging table in the plant protection database
# This script assumes a script called db-connection.R is present in the same folder as this script
# If the database is in a Docker container, it assumes that the container is running
# The database login information is contained in an .Renviron file (not part of the repo)

# ---- load libraries ---------------------------------
library(dplyr)     # For data wrangling (if necessary)
library(dbx)       # For postgres database connection
library(DBI)       # For interacting with postgres data

# ---- set up reusable variables ---------------------
# Change these as needed. 
# It is important that the destination table does not have the same name as an existing table in the database

data_file_path <- "data/species_pa_summary.csv"
destination_table <- "tmpspecies_pa_summary_2018"
destination_schema <- "speciesdata"

# ---- database connection --------------------------
source("db-connection.R")

# ---- read data ------------------------------------
dat <- read.csv(data_file_path, header = TRUE)
  
# ---- pass data to database ------------------------
dbWriteTable(db, name = DBI::Id(schema = destination_schema, table = destination_table), value = dat, overwrite = TRUE)
