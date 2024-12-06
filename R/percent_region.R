percent_region <- function(data_long) {

  # targets::tar_load("data_long")
  
  ## 9: summarized data to get percentage region per edition ----
  
  data_region <- data_long |>
    dplyr::mutate(region = dplyr::case_when(
      is.na(departement) ~ NA,
      departement == "35" ~ "Ille-et-Vilaine",
      departement %in% c("22","49","56") ~ "Bretagne-autres",
      departement %in% c("44","49","53","50") ~ "Bretagne-limitrophe",
      TRUE ~ "France-autres"
    ))
  
  data_summarized_departement <- data_region |>
    dplyr::filter(presence==1&!is.na(region)) |>
    dplyr::group_by(edition,region) |>
    dplyr::summarize(nb_people=dplyr::n()) |>
    dplyr::left_join(data_region |>
                       dplyr::filter(presence==1&!is.na(departement)) |>
                       dplyr::group_by(edition) |>
                       dplyr::summarize(nb_total_people=dplyr::n())) |>
    dplyr::mutate(percent = nb_people/nb_total_people)
  
  ## 10: get the plot for the data above ----

plot_file <- ggplot(data_summarized_departement, aes(x = factor(edition), y = percent, fill = region)) +
    geom_bar(stat = "identity", position = "dodge") +
    get_common_theme() +
    xlab("Count") +
    ylab("Edition number") +
    theme(legend.position = "top")
  
plot_file <- file.path("outputs", "percent_region.png")
ggsave(plot_file, width = 7, height = 7)
message(paste(plot_file,"has been created"))
  
return(data_region)  
  
}
  