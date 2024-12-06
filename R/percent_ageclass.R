percent_ageclass <- function(data_long) {
  
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

## 8: get the plot for the data above ----
plot_file <-  ggplot(data_mape, aes(x = factor(edition), y = percent, fill = age_class)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Count") +
  ylab("Edition number") +
  get_common_theme()+
  theme(legend.position = "top")

plot_file <- file.path("outputs", "percent_ageclass.png")

ggsave(plot_file, width = 7, height = 7)

message(paste(plot_file,"has been created"))

return(data_mape)

}
