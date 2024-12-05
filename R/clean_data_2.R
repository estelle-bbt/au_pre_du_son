clean_data_2 <- function(x){

# 2: formatting data  
data_2 <- x |>
  # change VRAI:FAUX by 1/0 for homogeneity
  dplyr::mutate_all(as.character) |> 
  dplyr::mutate(across(everything(), ~ dplyr::case_when(
    . == "VRAI" ~ "1",  
    . == "FAUX" ~ "0",  
    TRUE ~ .         
  )))  |>
  # remove accents in names
  dplyr::mutate(
    nom = stringi::stri_trans_general(nom, "Latin-ASCII"),  # Retirer les accents du nom
    prenom = stringi::stri_trans_general(prenom, "Latin-ASCII")  # Retirer les accents du prénom
  )

return(data_2)

}


