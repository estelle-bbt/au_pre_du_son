mean_age_per_edition <- function(data_long) {
  
  # targets::tar_load("data_long")
  source("R/get_common_theme.R")
  get_common_theme()
  
  plot_data <- data_long |>
    dplyr::filter(presence==1) 
  plot <-  ggplot(plot_data, aes(x=edition,y=age_today)) +
    common_theme +
    xlab("Edition number") +
    ylab("Age today") +
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), aes(color=edition), geom = "errorbar",width=0,linewidth=2,lineend="round",position=position_dodge(0.5)) +
    stat_summary(fun = mean, aes(fill=edition),geom = "point",size=3,shape=21,color="white",stroke=1,fill="gray30")
  
  ggsave(here::here("outputs", "plot_file.png"), plot, 
         width = 7, height = 7)
  
  return(file.path("outputs", "plot_file.png"))
  
}


