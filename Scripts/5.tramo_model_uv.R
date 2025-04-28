
# Entry 2: Univariate Tramo Model

# modelling loop ----------------------------------------------------------
# # create empty lists for loops

nowcast_list_uv <- list()
tramo_list_uv <- list()
dependent_list_uv <- list()
dependent_list_ts_uv <- list()
country_codes <- unique(target$country)
process <- 0  
  
  for (countrycode in country_codes){
      process <- process +1 
      

      ## dependent variable ----

      dep_var_start_date <- min(target$Date)
      dep_year <- year(dep_var_start_date) # starting year of regressor ts data
      dep_month <- month(dep_var_start_date) # starting month of regressor ts data
      
      data_dep_raw <- target %>% 
        filter(country==countrycode) %>% 
        arrange(Date) %>% 
        select(Date,value)
      
      # filter dependent variable
      data_dep_ts <-
        ts(data_dep_raw[,2],
           start = c(dep_year, dep_month),
           frequency = 12)
      
      ## model with tramo ----
      
      # specification
      spec <-
        tramoseats_spec(
          'RSA3',
          tradingdays.test = 'None',
          tradingdays.mauto = 'Unused',
          tradingdays.option = 'None'
        )
      

      tramo <- regarima(data_dep_ts, spec)
      
      last_obs_date <- max(data_dep_raw$Date)
      n_nowcast <- month_diff(last_obs_date,ym(nowcast_period))
      nowcast <- tramo$forecast[1:n_nowcast]

      final_df <- data.frame(
        Subject = Subject,
        Country = countrycode,
        entry = 2,
        nowcast_year = year(as.Date(as.yearmon(time(tramo$forecast))))[1:n_nowcast],
        nowcast_month = month(as.Date(as.yearmon(time(tramo$forecast))))[1:n_nowcast],
        nowcast = nowcast
      )
      
      
      nowcast_list_uv[[countrycode]] <- final_df
      tramo_list_uv[[countrycode]] <- tramo
      dependent_list_uv[[countrycode]] <- data_dep_raw
      dependent_list_ts_uv[[countrycode]] <- data_dep_ts
      cat(paste0("Modelling Process - ",process," - Country: ",countrycode," ---> ",
                 "Completion Rate: %",round(process/length(country_codes) * 100,2),"\n")) 
      
    }


# combine nowcast results into one table
nowcast_results_uv_all <- tibble(bind_rows(nowcast_list_uv)) |> 
  mutate(Model = "TRAMO_UV")

nowcast_results_uv <- nowcast_results_uv_all |> 
  filter(nowcast_month == month(ym(nowcast_period)))



