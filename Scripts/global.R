
# Load Packges ------------------------------------------------------------

packages <-
  c(
    "tidyverse",
    "RJDemetra",
    "lubridate",
    "openxlsx",
    "jsonlite",
    "zoo",
    "eurostat",
    "rsdmx"
  )

invisible(lapply(packages, function(pkg) library(pkg, character.only = TRUE)))

# get data from eurostat by using eurostat package, change method "libcurl" to "wininet"
options(download.file.method = "wininet")
options(scipen = 9999)
Subject <- "EXTRA_IMPORT"

# Trade Weights Data Import -------------------------------------------------------------

## foreign trade weights between 2014 and 2025 for every country -------------------------------------------------------------
## # This is necessary for nowcasting model will be used in entry 1
trade_weights <- read.xlsx("./Data/extra_import_trade_weights.xlsx") 
trade_partners <- unique(trade_weights$trade_partner)
countries <- trade_partners[!trade_partners %in% c("CN","JP","KR","NO","CH","TR","UK","US")]

## foreign trade weights between 2014 and 2025 for non-eu countries ----
# This is necessary for different nowcasting model will be used in entry 5
trade_weights_non_eu <- read.xlsx("./Data/extra_import_trade_weights_non_eu.xlsx")

# Currency codes of all countries ----
currency_codes <- data.frame(currency = c("BGN", "CZK","DKK", "HUF", "PLN", "RON", "SEK", "CHF", "NOK", "GBP", "TRY", "CNY", "JPY", "KRW", "USD"), 
                             geo = c("BG", "CZ","DK", "HU", "PL", "RO", "SE", "CH", "NO", "UK", "TR", "CN", "JP", "KR", "US"))

# TRAMO model metadata ----
model_info_tramo <- read.table("./Data/extra_import_models_tramo.csv", sep = ";", header = TRUE)

