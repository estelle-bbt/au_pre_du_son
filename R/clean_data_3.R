clean_data_3 <- function(data_2) {
  
  # targets::tar_load("data_2")
  
  
  # 3: remove raw with no informations and duplicate
  data_3 <- data_2 |> 
      dplyr::filter(!(is.na(nom) & is.na(prenom) & is.na(mail))) |>
      dplyr::distinct(nom,prenom,.keep_all=TRUE)
    return(data_3)
  
}