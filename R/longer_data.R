longer_data <- function(data_age) {
  
  # targets::tar_load("data_age")
  
  data_long <- data_age |> 
    tidyr::pivot_longer(starts_with("edition"),names_prefix="edition",
                        names_to="edition",values_to="presence") |>
    dplyr::mutate(year = dplyr::case_when(
      edition == "1" ~ 2017,
      edition == "2" ~ 2018,
      edition == "3" ~ 2019,
      edition == "4" ~ 2022,
      edition == "5" ~ 2023,
      edition == "6" ~ 2024
    )) |>
    dplyr::mutate(age_at_edition = year - year_of_birth) |>
    dplyr::mutate(age_class = dplyr::case_when(
      age_at_edition <= 25 ~ "25 or less",
      age_at_edition <= 35 ~ "between 25 and 35",
      age_at_edition <= 50 ~ "between 35 and 50",
      TRUE ~ "more than 50"
    ))
  
  
  return(data_long)
  
}