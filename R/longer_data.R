longer_data <- function(data_age) {
  
  # targets::tar_load("data_age")
  
  data_long <- data_age |> 
    tidyr::pivot_longer(starts_with("edition"),names_prefix="edition",
                        names_to="edition",values_to="presence")
  
  
  return(data_long)
  
}