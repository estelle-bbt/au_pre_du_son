get_common_theme <- function() {

#library(ggplot2)    
common_theme <- theme(axis.text.x = element_text(size=10,angle=45,hjust=1),
                        axis.text.y = element_text(size=8,margin = margin(t = 0, r = 5, b = 0, l = 5)),
                        legend.position = "none",
                        plot.background = element_blank(),
                        panel.grid.major=element_line(color="gray90"),
                        panel.grid.major.x = element_blank() ,
                        plot.margin = margin(t=5, r=0, b=0, l=0),
                        strip.background = element_rect(colour="black", 
                                                        linewidth=1.5, linetype="solid"),
                        panel.background = element_rect(colour="black", fill="white",linewidth=1),
                        axis.title.y = element_text(size=10))
  return(common_theme)
  
}
