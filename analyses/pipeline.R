library(targets)
library(ggplot2)
tar_source()
#source("R/get_common_theme.R")

list(
  # Make the workflow depends on the raw data file
  tar_target(name = raw_data_file, 
             command = here::here("data", "data_festival_au_pre_du_son.csv"), 
             format = "file") 
  
  # Read the data and return a data.frame
  ,tar_target(name = raw_data, command = read.csv(raw_data_file))
  
  # Transform the data
  ,tar_target(data_1, clean_data_1(raw_data))

  # Transform the data again
  ,tar_target(data_2, clean_data_2(data_1))

  # Transform the data again and again
  ,tar_target(data_3, clean_data_3(data_2))
  
  # Transform age data
  ,tar_target(data_age, transform_age_format(data_3))

  # Longer data 
  ,tar_target(data_long, longer_data(data_age))
  
  # Plot mean age per edition
  ,tar_target(plott_mape, mean_age_per_edition(data_long), format = "file")
  
  # Percent age per edition
  ,tar_target(data_mape, percent_ageclass(data_long))

  # Data and plot per region
  ,tar_target(data_region, percent_region(data_long))
  
  # Model and quantify effects
  ,tar_target(sum_lm, modelit(data_region))
  
)
