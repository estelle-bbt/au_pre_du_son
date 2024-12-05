transform_age_format <- function(data_3) {
  
  # targets::tar_load("data_3")
  
  data_age <- data_3 |> 
  dplyr::mutate(year_of_birth = lubridate::year(
    lubridate::dmy(datedenaissance))) |>
  dplyr::mutate(age_today = 2024-year_of_birth) |>
    dplyr::mutate(age_class = dplyr::case_when(
      age_today <= 25 ~ "25 or less",
      age_today <= 35 ~ "between 25 and 35",
      age_today <= 50 ~ "between 35 and 50",
      TRUE ~ "more than 50"
    ))
  


return(data_age)

}