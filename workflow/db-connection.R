library(DBI)
library(logger)
library(RPostgres)

readRenviron(".Renviron")

db_connect <- function() {
  log_info("Attempting to make database connection.")

  connect <- dbConnect(
    Postgres(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )

  if(!is.null(connect)) log_success("Connected to database.")
  connect
}

db <- db_connect()