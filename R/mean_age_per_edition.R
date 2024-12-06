mean_age_per_edition <- function(data_long) {

  #library(ggplot2)

  # targets::tar_load("data_long")

  plot_data <- data_long[data_long$presence==1, ]

  
  #message("here")
  plott_mape <-  ggplot(plot_data, aes(x=edition,y=age_today)) +
    xlab("Edition number") +
    ylab("Age today") +
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
                 aes(color=edition), geom = "errorbar",width=0,linewidth=2,
                 lineend="round",position=position_dodge(0.5)) +
    stat_summary(fun = mean, aes(fill=edition),geom = "point",size=3,
                 shape=21,color="yellow",stroke=1,fill="gray30")+
    get_common_theme()

  
  #message("after ggplot")
  plot_file <- file.path("outputs", "mean_age_per_edition.png")

  ggsave(plot_file, plott_mape,
         width = 7, height = 7)



  #write.csv(data_long, "outputs/data_long.csv")

  return(plot_file)
  #"outputs/data_long.csv"
  #plott_mape
}


