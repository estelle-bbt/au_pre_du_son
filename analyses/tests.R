
data_raw <- readr::read_csv("data_festival_au_pre_du_son.csv")
str(data_raw)

data <- data_raw |> 
  # remove uppercase
  dplyr::rename_with(~ gsub("^([A-Z])", "\\L\\1", ., perl = TRUE)) |>
  # remove accent
  dplyr::rename_with(~ stringi::stri_trans_general(., "Latin-ASCII")) |>
  # remove space and replace by underscore
  dplyr::rename_with(~ gsub(" ", "_", .)) |>
  # get standard names (remove backtick)
  dplyr::rename_with(~ make.names(., unique = TRUE)) |>
  # remove dot
  dplyr::rename_with(~ gsub("\\.", "", .)) |>
  # remove two specifics columns
  dplyr::rename(age_edition_5 = ageau_moment_de_lachat_5) |>
  dplyr::rename(connaissance_festival = question_4Comment_astu_connu_le_festival_)


data |>
  # change VRAI:FAUX by 1/0 for homogeneity
  dplyr::mutate_all(as.character) |> 
  dplyr::mutate(across(everything(), ~ dplyr::case_when(
    . == "VRAI" ~ "1",  
    . == "FAUX" ~ "0",  
    TRUE ~ .         
  ))) 

str(data)
