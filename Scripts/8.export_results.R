

# Export nowcast results to csv ----

nowcast_results <- rbind(nowcast_results_mv1,nowcast_results_uv,nowcast_results_avg,nowcast_results_mv2 )
write.table(nowcast_results,paste0("./Results/", nowcast_period,"/Nowcasting_Results.csv"), sep = ";",dec=".", row.names=FALSE)

# Export nowcast results to json ----

json_list <- fromJSON("./point_estimates.json")

entry_list <- nowcast_results %>%
  select(entry) %>%
  unique() %>%
  pull()


for (entry_num in entry_list) {
  for (country in country_codes) {
    nowcast <- nowcast_results %>%
      filter(entry == entry_num,
             Country == country) %>%
      select(nowcast) %>%
      pull()
    
    json_name <- paste0("entry_", entry_num)
    json_list[[json_name]][[country]] <- round(nowcast,1)
    
  }
}

write_json(json_list,paste0("./Results/",nowcast_period,"/point_estimates.json"),auto_unbox = TRUE,null="null", pretty = TRUE)

