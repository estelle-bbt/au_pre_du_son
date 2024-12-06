modelit <- function(data_region){
  
  targets::tar_load("data_region")
  
  data_only_presence <- data_region |>
    dplyr::filter(presence==1) |>
    dplyr::mutate(region=relevel(as.factor(region), ref = "Ille-et-Vilaine"))
  
  
  model_age <- stats::lm(data=data_only_presence, age_at_edition ~ edition+region)
  car::Anova(model_age)
  sum_lm <- summary(model_age) 
  # 
  # model_age <- stats::lm(data=data_only_presence, age_at_edition ~ year*region)
  # car::Anova(model_age)
  # sum_lm <- summary(model_age) 
  
  return(sum_lm)
}