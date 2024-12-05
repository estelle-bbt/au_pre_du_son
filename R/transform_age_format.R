transform_age_format <- function(data_3) {
  
  # targets::tar_load("data_3")
  
  data_age <- data_3 |> 
  dplyr::mutate(year_of_birth = lubridate::year(
    lubridate::dmy(datedenaissance))) |>
  dplyr::mutate(age_today = 2024-year_of_birth)

return(data_age)

}