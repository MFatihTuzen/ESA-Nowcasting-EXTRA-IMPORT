
# Entry 5: Multivariate Tramo Model 2, regressors created with trade weights of non-eu countries 

# modelling loop ----------------------------------------------------------
rel_dp2 <- import_data(indicator = "rel_dp2", period = nowcast_period)
er_star2 <- import_data(indicator = "er_star2", period = nowcast_period)
regressors2 <- rbind(rel_dp2,er_star2,y)

nowcast_list_mv2 <- list()
tramo_list_mv2 <- list()
dependent_list_mv2 <- list()
dependent_list_ts_mv2 <- list()
regressor_list_mv2 <- list()
regressor_list_ts_mv2 <- list()
country_codes <- unique(target$country)
model_info_tramo2 <- model_info_tramo |> 
  mutate(regressor = case_when(
    regressor == "rel_dp" ~ "rel_dp2",
    regressor == "er_star" ~ "er_star2",
    TRUE ~ "y"
  ))

process <- 0  

for (countrycode in country_codes){
  process <- process +1 
  
  reg_names <- model_info_tramo2 |> 
    filter(Country == countrycode) |> 
    select(regressor) |> 
    pull()
  
  
  ## regressors ----
  
  reg_lag_ts_list <- list()
  
  for(reg in reg_names){
    
    data_reg_raw <- regressors2 |> 
      filter(country == countrycode, indicator == reg) |> 
      na.omit() |>  
      arrange(Date)
    
    reg_var_start_date <- min(regressors2$Date)
    data_ts <- ts(data_reg_raw[,"value"],
                  start = c(year(reg_var_start_date), month(reg_var_start_date)),
                  frequency = 12)
    
    lag_value <-  model_info_tramo2 |> 
      filter(Country == countrycode,
             regressor == reg) |>
      select(lag) |> 
      pull()
    
    data_ts_lag <- stats::lag(data_ts,-lag_value)
    ts_name <- paste0(reg,"_",lag_value)
    reg_lag_ts_list[[ts_name]] <- data_ts_lag
    
  }
  
  data_reg_ts <- do.call(ts.union, reg_lag_ts_list) # combine ts objects into a multiple ts object
  
  ## dependent variable ----
  
  # the more lag, the more data is truncated from the beginning of regressor ts data.
  # row number of starting point for regressor ts data.
  max_lag <- model_info_tramo2 |> 
    filter(Country == countrycode) |>
    summarise(lag = max(lag)) |> 
    pull()
  
  dep_year <- year(reg_var_start_date %m+% months(max_lag)) # starting year of regressor ts data
  dep_month <- month(reg_var_start_date %m+% months(max_lag)) # starting month of regressor ts data
  
  data_dep_raw <- target %>% 
    filter(country==countrycode) %>% 
    arrange(Date) %>% 
    select(Date,value)
  
  # filter dependent variable according to lag length
  data_dep_ts <-
    ts(data_dep_raw[(max_lag + 1):nrow(data_dep_raw),2],
       start = c(dep_year, dep_month),
       frequency = 12)
  
  ## model with tramo ----
  
  # specification
  spec <-
    tramoseats_spec(
      'RSA3',
      tradingdays.test = 'None',
      tradingdays.mauto = 'Unused',
      tradingdays.option = 'None',
      usrdef.varEnabled = TRUE,
      usrdef.varType = 'Series',
      usrdef.var = data_reg_ts,
    )
  
  
  tramo <- regarima(data_dep_ts, spec)
  
  last_obs_date <- max(data_dep_raw$Date)
  n_nowcast <- month_diff(last_obs_date,ym(nowcast_period))
  nowcast <- tramo$forecast[1:n_nowcast]
  
  final_df <- data.frame(
    Subject = Subject,
    Country = countrycode,
    entry = 5,
    nowcast_year = year(as.Date(as.yearmon(time(tramo$forecast))))[1:n_nowcast],
    nowcast_month = month(as.Date(as.yearmon(time(tramo$forecast))))[1:n_nowcast],
    nowcast = nowcast
  )
  
  
  nowcast_list_mv2[[countrycode]] <- final_df
  tramo_list_mv2[[countrycode]] <- tramo
  dependent_list_mv2[[countrycode]] <- data_dep_raw
  dependent_list_ts_mv2[[countrycode]] <- data_dep_ts
  regressor_list_mv2[[countrycode]] <- data_reg_raw
  regressor_list_ts_mv2[[countrycode]] <- data_reg_ts
  
  cat(paste0("Modelling Process - ",process," - Country: ",countrycode," ---> ",
             "Completion Rate: %",round(process/length(country_codes) * 100,2),"\n")) 
  
}


nowcast_results_mv2_all <- tibble(bind_rows(nowcast_list_mv2)) |> 
  mutate(Model = "TRAMO_MV2")

nowcast_results_mv2 <- nowcast_results_mv2_all |>
  filter(nowcast_month == month(ym(nowcast_period))) 

