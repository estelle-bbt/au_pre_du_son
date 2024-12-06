modelit <- function(data_region){
  
  # targets::tar_load("data_region")
  
  data_only_presence <- data_region |>
    dplyr::filter(presence==1)
  
  model_age <- stats::lm(data=data_only_presence, age_today ~ edition+region)
  car::Anova(model_age)
  sum_lm <- summary(model_age) 
  
  return(sum_lm)
}