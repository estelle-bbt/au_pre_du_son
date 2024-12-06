percent_age_class <- function(data_long) {
  
  # targets::tar_load("data_long")
  
  

## 7: summarized data to get percentage class age per edition ----
data_mape <- data_long |>
  dplyr::filter(presence==1) |>
  dplyr::group_by(edition,age_class) |>
  dplyr::summarize(nb_people=dplyr::n()) |>
  dplyr::left_join(data_long |>
                     dplyr::filter(presence==1) |>
                     dplyr::group_by(edition) |>
                     dplyr::summarize(nb_total_people=dplyr::n())) |>
  dplyr::mutate(percent = nb_people/nb_total_people)

return(data_mape)

}
