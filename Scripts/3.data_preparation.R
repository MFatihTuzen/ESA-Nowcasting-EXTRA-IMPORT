
# y (Industrial Production Index (IPI)) ------------------------------------------------------------------

# impute missing data of hicp for last period

last_data_ipi <- max(ipi$TIME_PERIOD)
non_missing_countries_ipi <- ipi |> 
  filter(TIME_PERIOD == last_data_ipi) |> 
  select(geo) |> 
  pull()

ipi_imputed <- if (length(non_missing_countries_ipi) < length(countries)) {
  
  ipi |> 
    pivot_wider(id_cols = TIME_PERIOD, names_from = geo, values_from = values) |> 
    mutate(across(-c(TIME_PERIOD), ~ na.locf(.))) |> 
    pivot_longer(cols = -c(TIME_PERIOD), names_to = "geo", values_to = "values") |> 
    mutate(indicator = "IPI")
  
} else {
  
  ipi
}


# y (Industrial Production Index (IPI))
ipi_rebase <- ipi_imputed |>
  group_by(geo) |> 
  mutate(Year = year(TIME_PERIOD),
         index = values / mean(values[Year == 2014]) * 100) |>  # rebase data to 2014=100
  ungroup() |> 
  select(country_code = geo,TIME_PERIOD,index,Year) |> 
  pivot_wider(id_cols = TIME_PERIOD,
              names_from = country_code,
              values_from = index) |>
  select(Date = TIME_PERIOD , all_of(countries)) |>
  arrange(Date) |>
  mutate(Date = paste0(year(Date), "M", month(Date)))


## er_star (exchange rates) -----------------------------------------------------------------

er_rebase <- exc_rate |> 
  mutate(adj_values = 1 / values) |> # get euro / national currency values
  group_by(geo) |> 
  mutate(index = adj_values / mean(adj_values[year(TIME_PERIOD) == 2014]) * 100) |> # rebase data to 2014=100
  ungroup() |> 
  mutate(Year = year(TIME_PERIOD)) |> 
  select(country_code = geo,TIME_PERIOD,index,Year)


# get exchange rates by weighting foreign trade for every country

er_star <- get_star_reg(er_rebase,trade_weights)


## rel_dp (Relative Harmonised Index of Consumer Prices (HICP)) ------------------------------------------------------------------

# impute missing data of hicp for last periods
last_data_hicp <- max(hicp$TIME_PERIOD)
non_missing_countries_hicp <- hicp |> 
  filter(TIME_PERIOD == last_data_hicp) |> 
  select(geo) |> 
  pull()


hicp_imputed <- if (length(non_missing_countries_hicp) < length(trade_partners)) {
  
  hicp |> 
    pivot_wider(id_cols = TIME_PERIOD, names_from = geo, values_from = values) |> 
    mutate(across(-c(TIME_PERIOD), ~ na.locf(.))) |> 
    pivot_longer(cols = -c(TIME_PERIOD), names_to = "geo", values_to = "values") |> 
    mutate(indicator = "HICP")
  
} else {
  
  hicp
}

# dp_star (Harmonised Index of Consumer Prices (HICP))
hicp_rebase <- hicp_imputed |>
  group_by(geo) |> 
  mutate(Year = year(TIME_PERIOD),
         index = values / mean(values[Year == 2014]) * 100) |>  # rebase data to 2014=100
  ungroup() |> 
  select(country_code = geo,TIME_PERIOD,index,Year)

# get Harmonised Index of Consumer Prices by weighting foreign trade for every country

rel_dp <- get_star_reg(hicp_rebase,trade_weights) |> 
  pivot_longer(cols = -c(Date), names_to = "country_code", values_to = "index") |>
  rename(index_star = index) |> 
  left_join(hicp_rebase |> 
              mutate(Date = paste0(year(TIME_PERIOD), "M", month(TIME_PERIOD))) |> 
              select(-c(Year,TIME_PERIOD)) |> 
              rename(index_hicp = index),
            by = c("Date" = "Date", "country_code" = "country_code" )) |> 
  mutate(rel_dp = index_hicp / index_star) |> 
  pivot_wider(id_cols = Date,
              names_from = country_code,
              values_from = rel_dp) |>
  select(Date, all_of(countries))


## er_star2 (exchange rates) -----------------------------------------------------------------

# get exchange rates by weighting foreign trade for non-eu countries

er_star2 <- get_star_reg(er_rebase,trade_weights_non_eu)

## rel_dp2 (Relative Harmonised Index of Consumer Prices (HICP)) ------------------------------------------------------------------

# get Harmonised Index of Consumer Prices by weighting foreign trade for non-eu countries

rel_dp2 <- get_star_reg(hicp_rebase,trade_weights_non_eu) |> 
  pivot_longer(cols = -c(Date), names_to = "country_code", values_to = "index") |>
  rename(index_star = index) |> 
  left_join(hicp_rebase |> 
              mutate(Date = paste0(year(TIME_PERIOD), "M", month(TIME_PERIOD))) |> 
              select(-c(Year,TIME_PERIOD)) |> 
              rename(index_hicp = index),
            by = c("Date" = "Date", "country_code" = "country_code" )) |> 
  mutate(rel_dp = index_hicp / index_star) |> 
  pivot_wider(id_cols = Date,
              names_from = country_code,
              values_from = rel_dp) |>
  select(Date, all_of(countries))


# EXTRA-IMPORT ----

target <- extra_imp |> 
  pivot_wider(id_cols = TIME_PERIOD,
              names_from = geo,
              values_from = values) |>
  select(Date = TIME_PERIOD , all_of(countries)) |>
  arrange(Date) |>
  mutate(Date = paste0(year(Date), "M", month(Date)))


# export regressors ----

# Create new workbook
data_export <- createWorkbook()

# Add worksheets
addWorksheet(data_export, "target")
addWorksheet(data_export, "y")
addWorksheet(data_export, "er_star")
addWorksheet(data_export, "er_star2")
addWorksheet(data_export, "rel_dp")
addWorksheet(data_export, "rel_dp2")


# Write data to worksheets
writeData(data_export, "target", target)
writeData(data_export, "y", ipi_rebase)
writeData(data_export, "er_star", er_star)
writeData(data_export, "er_star2", er_star2)
writeData(data_export, "rel_dp", rel_dp)
writeData(data_export, "rel_dp2", rel_dp2)

saveWorkbook(data_export, paste0("./Data/",nowcast_period,"/data.xlsx"), overwrite = TRUE)

file.copy(from = paste0("./Data/",nowcast_period,"/data.xlsx"),
          to = paste0("./Results/",nowcast_period,"/data_",Sys.Date(),".xlsx"),
          overwrite = TRUE)





