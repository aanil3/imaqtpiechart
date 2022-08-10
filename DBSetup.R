require(lubridate)
require(stringr)
require(data.table)
library(DBI)
require(RSQLite)
library(readr)
library(sqldf)

loldb <- dbConnect(RSQLite::SQLite(), "portal.sqlite")

# Pastes together a string for the url to each file based on the year you want to download and today's date.
import_data <- function(current_year){
  fileUrl <- 
    paste(
      "https://oracleselixir-downloadable-match-data.s3-us-west-2.amazonaws.com/",
      current_year,
      "_LoL_esports_match_data_from_OraclesElixir_",
      str_remove_all(today(), pattern = "-"),
      ".csv",
      sep = ""
    )
  value = as.character(as.integer(str_remove_all(today(), pattern = "-")) - 1)
  prevUrl <- 
    paste(
      "https://oracleselixir-downloadable-match-data.s3-us-west-2.amazonaws.com/",
      current_year,
      "_LoL_esports_match_data_from_OraclesElixir_",
      str_remove_all(value, pattern = "-"),
      ".csv",
      sep = ""
    )
  data <-
    tryCatch({
      fread(fileUrl)
      },
      error = function(e){
        fread(prevUrl)
      })
  season_name = paste("s", as.integer(current_year) - 2010, sep="")
  dbWriteTable(loldb, season_name, data, overwrite = TRUE)
}

# year defines the year you want to download
current_year = format(Sys.Date(), "%Y")
import_data(current_year)
dbExecute(loldb, 'DROP TABLE IF EXISTS imaqtpie_db')
query <- paste("CREATE TABLE imaqtpie_db AS SELECT * FROM old_data UNION SELECT * FROM ", "s", as.integer(current_year) - 2010, sep="")
lol_df <- dbGetQuery(loldb, query)
dbGetQuery(loldb, 'SELECT * FROM imaqtpie_db')
dbListTables(loldb)
sqldf("vacuum", dbname = "portal.sqlite")
dbDisconnect(loldb)
