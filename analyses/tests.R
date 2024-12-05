library(ggplot2)

## #1: import data ----
data_raw <- readr::read_csv("data/data_festival_au_pre_du_son.csv")

## #2: formatting column names ----
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

## #2: formatting data ----
data_clean <- data |>
  # change VRAI/FAUX by 1/0 for homogeneity
  dplyr::mutate_all(as.character) |> 
  dplyr::mutate(across(everything(), ~ dplyr::case_when(
    . == "VRAI" ~ "1",  
    . == "FAUX" ~ "0",  
    TRUE ~ .         
  ))) |>
  # remove accents in names
  dplyr::mutate(
    nom = stringi::stri_trans_general(nom, "Latin-ASCII"),  # Retirer les accents du nom
    prenom = stringi::stri_trans_general(prenom, "Latin-ASCII")  # Retirer les accents du prénom
  )

# # 2b: cleaning connaissance_festival -> check this latter (text mining?)
# unique(data_clean$connaissance_festival)

## #3: remove raw with no informations and duplicate
data_filter <- data_clean |>
  dplyr::filter(!(is.na(nom) & is.na(prenom) & is.na(mail))) |>
  dplyr::distinct(nom,prenom,.keep_all=TRUE)
  
## #4 : get age of the participants ----

# CHECK
# check_data_with_age <- data_clean |>
#   dplyr::filter(!is.na(date_de_naissance)) |>
#   dplyr::mutate(
#     # Tenter la conversion avec dmy() et détecter les erreurs
#     year_of_birth = lubridate::dmy(date_de_naissance),
#     
#     # Marquer les dates invalides (NA) et garder les valeurs valides
#     is_valid = !is.na(year_of_birth)
#   )
# # Afficher les lignes avec des dates invalides
# check_data_with_age |> dplyr::filter(!is_valid) # just one invalid date of birth, okay

data_with_age <- data_filter |> 
  dplyr::mutate(year_of_birth = lubridate::year(lubridate::dmy(date_de_naissance))) |>
  dplyr::mutate(age_today = 2024-year_of_birth)

## 4: longering data for presence/absence editions ----
data_longer <- data_clean |>
  tidyr::pivot_longer(starts_with("edition_"),names_prefix="edition_",names_to="edition",values_to="presence")

