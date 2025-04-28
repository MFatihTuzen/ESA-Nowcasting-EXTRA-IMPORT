
# get star regressors ----
# get foreign variables by using trade weights for every country

get_star_reg <- function(rebased_data,trade_weights){
  
  rebased_data |>
    left_join(trade_weights,
              by = c("country_code" = "trade_partner", "Year" = "year")) |>
    rename(trade_partner = country_code) |>
    pivot_longer(
      cols = -c(TIME_PERIOD, Year, trade_partner, index, country),
      names_to = "country_code",
      values_to = "weight"
    ) |>
    mutate(weighted_index = index * weight) |>
    group_by(TIME_PERIOD, country_code) |>
    summarise(total_weighted_index = sum(weighted_index)) |>
    pivot_wider(id_cols = TIME_PERIOD,
                names_from = country_code,
                values_from = total_weighted_index) |>
    select(Date = TIME_PERIOD , all_of(countries)) |>
    arrange(Date) |>
    mutate(Date = paste0(year(Date), "M", month(Date)))

}


# import data from folder ----
import_data <- function(indicator, period){
  
  file_path <- paste0("Data/",period,"/data.xlsx")  
  openxlsx::read.xlsx(file_path, sheet = indicator) |> 
    mutate(Date = parse_date(Date, format = "%YM%m")) |>
    pivot_longer(cols = -c("Date"),
                 names_to = "country",
                 values_to = "value") |>
    mutate(indicator = indicator )
  
}

# get month diff between two dates ----
# get number of months between nowcast period and last observation period of target data

month_diff <- function(date1, date2) {
  (year(date2) - year(date1)) * 12 + (month(date2) - month(date1))
}


