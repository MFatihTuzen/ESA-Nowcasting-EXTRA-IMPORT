
# Entry 1: Multivarite Tramo Model 1, regressors created with trade weights of all countries 
# import target and regressors

target <- import_data(indicator = "target", period = nowcast_period)
y <- import_data(indicator = "y", period = nowcast_period)
rel_dp <- import_data(indicator = "rel_dp", period = nowcast_period)
er_star <- import_data(indicator = "er_star", period = nowcast_period)
regressors <- rbind(rel_dp,er_star,y)

# modelling loop ----------------------------------------------------------
# create empty lists for loops
nowcast_list_mv <- list()
tramo_list_mv <- list()
dependent_list_mv <- list()
dependent_list_ts_mv <- list()
regressor_list_mv <- list()
regressor_list_ts_mv <- list()
country_codes <- unique(target$country)

process <- 0  
  
  for (countrycode in country_codes){
      process <- process +1 
      
      reg_names <- model_info_tramo |> 
        filter(Country == countrycode) |> 
        select(regressor) |> 
        pull()
      
      
      ## regressors ----
      
      reg_lag_ts_list <- list()
      
      for(reg in reg_names){
        
        data_reg_raw <- regressors |> 
          filter(country == countrycode, indicator == reg) |> 
          na.omit() |>  
          arrange(Date)
        
        reg_var_start_date <- min(regressors$Date)
        data_ts <- ts(data_reg_raw[,"value"],
                      start = c(year(reg_var_start_date), month(reg_var_start_date)),
                      frequency = 12)
        
        lag_value <-  model_info_tramo |> 
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
      max_lag <- model_info_tramo |> 
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
        entry = 1,
        nowcast_year = year(as.Date(as.yearmon(time(tramo$forecast))))[1:n_nowcast],
        nowcast_month = month(as.Date(as.yearmon(time(tramo$forecast))))[1:n_nowcast],
        nowcast = nowcast
      )
      
      
      nowcast_list_mv[[countrycode]] <- final_df
      tramo_list_mv[[countrycode]] <- tramo
      dependent_list_mv[[countrycode]] <- data_dep_raw
      dependent_list_ts_mv[[countrycode]] <- data_dep_ts
      regressor_list_mv[[countrycode]] <- data_reg_raw
      regressor_list_ts_mv[[countrycode]] <- data_reg_ts

      cat(paste0("Modelling Process - ",process," - Country: ",countrycode," ---> ",
                 "Completion Rate: %",round(process/length(country_codes) * 100,2),"\n")) 
      
    }


# combine nowcast results into one table

nowcast_results_mv1_all <- tibble(bind_rows(nowcast_list_mv)) |> 
  mutate(Model = "TRAMO_MV1")

nowcast_results_mv1 <- nowcast_results_mv1_all |>
  filter(nowcast_month == month(ym(nowcast_period))) 


