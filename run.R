
renv::restore()
source("scripts/main.R")

# get_nowcast values of each country. It depends on period of nowcast period.
get_nowcast(nowcast_period = "2025-03")
