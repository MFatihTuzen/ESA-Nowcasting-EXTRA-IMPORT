
# Entry 3: Average of Multivarite Tramo 1  and Univariate Tramo Models (mean of entry1 and entry2)

# average tramo models ----

nowcast_results_tramo <- rbind(nowcast_results_mv1,nowcast_results_uv)

nowcast_results_avg <-nowcast_results_tramo |>  
  group_by(Country) |> 
  mutate(avg_models=mean(nowcast)) |> 
  ungroup() |> 
  select(-nowcast) |> 
  rename(nowcast=avg_models) |>  
  mutate(Model = "TRAMO_AVG",
         entry = 3) |> 
  unique() |> 
  select(Subject:nowcast_month,nowcast,Model)

nowcast_results_tramo_all <- rbind(nowcast_results_mv1_all,nowcast_results_uv_all)

nowcast_results_avg_all <-nowcast_results_tramo_all |>  
  group_by(Country,nowcast_month) |> 
  mutate(avg_models=mean(nowcast)) |> 
  ungroup() |> 
  select(-nowcast) |> 
  rename(nowcast=avg_models) |>  
  mutate(Model = "TRAMO_AVG",
         entry = 3) |> 
  unique() |> 
  select(Subject:nowcast_month,nowcast,Model)
