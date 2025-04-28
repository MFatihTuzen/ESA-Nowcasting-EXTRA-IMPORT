
# Eurostat and OECD Data Import -------------------------------------------------------------

## Target data -------------------------------------------------------------

extra_imp <- get_eurostat("ei_eteu27_2020_m") %>%
  filter(unit== "MIO-EUR-NSA", indic == "ET-T", partner == "EXT_EU27_2020", stk_flow == "IMP", geo %in% countries, year(TIME_PERIOD) >= 2014) |> 
  mutate(indicator = "EXTRA_IMP") |> 
  select(geo, TIME_PERIOD, values,indicator)

## Regressor data ----------------------------------------------------------

### industrial production index -------------------------------------------------------------
ipi_without_IE <- get_eurostat("sts_inpr_m") %>%
  filter(unit == "I21", s_adj == "NSA", geo %in% countries, nace_r2 == "B-D", year(TIME_PERIOD) >= 2014) %>%
  select(geo, TIME_PERIOD, values) %>%
  mutate(indicator = "IPI") |> 
  filter(!geo %in% "IE")

# get ireland ipi non-seasonal data from oecd

url_IE <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.STES,DSD_STES@DF_INDSERV,4.2/IRL.M.PRVM.IX.BTE.N...?startPeriod=2014-01&dimensionAtObservation=AllDimensions"
sdmx_IE <- readSDMX(url_IE)
ipi_IE <- as.data.frame(sdmx_IE) |> 
  as_tibble() |> 
  select(geo = REF_AREA,TIME_PERIOD,values = obsValue) |> 
  mutate(geo = "IE",
         TIME_PERIOD = ym(TIME_PERIOD),
         indicator = "IPI") |> 
  arrange(TIME_PERIOD)


ipi <- rbind(ipi_without_IE,ipi_IE)

### consumer price index -------------------------------------------------------------

countries_hicp <- c(countries, "TR","NO","US","CH") 
hicp_eurostat <- get_eurostat("prc_hicp_midx") %>%
  filter(unit == "I15", coicop == "CP00", geo %in% countries_hicp, year(TIME_PERIOD) >= 2014) %>%
  select(geo, TIME_PERIOD, values) %>%
  mutate(indicator = "HICP")

# get hicp of other countries
url_hicp_other <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_PRICES@DF_PRICES_ALL,1.0/KOR+CHN+GBR.M.N.CPI.IX._T.N._Z?startPeriod=2014-01&endPeriod=2024-12&dimensionAtObservation=AllDimensions"
sdmx_hicp_other <- readSDMX(url_hicp_other)
hicp_other <- as.data.frame(sdmx_hicp_other) |>
  as_tibble() |>
  select(geo = REF_AREA, TIME_PERIOD, values = obsValue) |>
  mutate(
    geo = case_when(
      geo == "KOR" ~ "KR",
      geo == "CHN" ~ "CN",
      geo == "GBR" ~ "UK"
    ),
    TIME_PERIOD = ym(TIME_PERIOD),
    indicator = "HICP"
  ) |>
  arrange(TIME_PERIOD)

# get hicp of JAPAN
url_hicp_jpn <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_PRICES_COICOP2018@DF_PRICES_C2018_ALL,1.0/JPN.M.N.CPI.IX._T.N.?startPeriod=2014-01&dimensionAtObservation=AllDimensions"
sdmx_hicp_jpn <- readSDMX(url_hicp_jpn)
hicp_jpn <- as.data.frame(sdmx_hicp_jpn) |>
  as_tibble() |>
  select(geo = REF_AREA, TIME_PERIOD, values = obsValue) |>
  mutate(
    geo = "JP",
    TIME_PERIOD = ym(TIME_PERIOD),
    indicator = "HICP"
  ) |>
  arrange(TIME_PERIOD)

hicp <- rbind(hicp_eurostat,hicp_other,hicp_jpn)


### exchange rates -------------------------------------------------------------

exc_rate_m <- get_eurostat("ert_bil_eur_m") %>%
  filter(statinfo == "AVG", year(TIME_PERIOD) >= 2014) %>%
  right_join(currency_codes, by = "currency") %>%
  select(geo, TIME_PERIOD, values) %>%
  mutate(indicator = "EXC_RATE")

exc_rate_d_time_interval <- na.omit(as.character(make_date(year = year(ym(nowcast_period)),month = month(ym(nowcast_period)), day = 1:31)))
exc_rate_d <- get_eurostat_json("ert_bil_eur_d", filters = list(time = exc_rate_d_time_interval)) |> 
  right_join(currency_codes, by = "currency") %>%
  select(geo, TIME_PERIOD = time, values) %>%
  mutate(indicator = "EXC_RATE") |> 
  filter(is.na(values)==FALSE)

exc_rate_conversion_prev_periods <- get_eurostat("ert_bil_conv_m") |> 
  filter(year(TIME_PERIOD) >= 2014, statinfo == "AVG") |> 
  select(geo, TIME_PERIOD, values) |> 
  mutate(indicator = "EXC_RATE")

exc_rate_conversion_last_period <- data.frame(
  geo = unique(exc_rate_conversion_prev_periods$geo),
  TIME_PERIOD = ym(nowcast_period),
  values = 1, 
  indicator = "EXC_RATE"
)

exc_rate_conversion <- rbind(exc_rate_conversion_prev_periods,exc_rate_conversion_last_period)

exc_rate <- exc_rate_d |>
  group_by(geo) |> 
  summarise(values = mean(values)) |> 
  ungroup() |> 
  mutate(TIME_PERIOD = ym(nowcast_period),
         indicator = "EXC_RATE") |> 
  select(geo,TIME_PERIOD,values,indicator) |> 
  rbind(exc_rate_m,exc_rate_conversion) |> 
  arrange(geo,TIME_PERIOD)

# Export data -------------------------------------------------------------

data_all <- rbind(extra_imp,ipi,hicp,exc_rate)
write.table(data_all,paste0("./Data/",nowcast_period,"/raw_data_",Sys.Date(),".csv"), row.names = FALSE, sep = ";", dec = ".")

