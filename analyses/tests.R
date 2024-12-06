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
  dplyr::mutate(age_today = 2024-year_of_birth) |>
  dplyr::mutate(age_class = dplyr::case_when(
    age_today <= 25 ~ "25 or less",
    age_today <= 35 ~ "between 25 and 35",
    age_today <= 50 ~ "between 35 and 50",
    TRUE ~ "more than 50"
  ))

## 4: longering data for presence/absence editions ----
data_longer <- data_with_age |>
  tidyr::pivot_longer(starts_with("edition"),names_prefix="edition_",names_to="edition",values_to="presence")

## 5: get the common theme ----
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

## 6: mean and sd age per edition ----
data_longer |>
  dplyr::filter(presence==1) |>
  ggplot(aes(x=edition,y=age_today)) +
  common_theme +
  xlab("Edition number") +
  ylab("Age today") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), aes(color=edition), geom = "errorbar",width=0,linewidth=2,lineend="round",position=position_dodge(0.5)) +
  stat_summary(fun = mean, aes(fill=edition),geom = "point",size=3,shape=21,color="white",stroke=1,fill="gray30")

## 7: summarized data to get percentage class age per edition ----
data_summarized <- data_longer |>
  dplyr::filter(presence==1) |>
  dplyr::group_by(edition,age_class) |>
  dplyr::summarize(nb_people=dplyr::n()) |>
  dplyr::left_join(data_longer |>
                     dplyr::filter(presence==1) |>
                     dplyr::group_by(edition) |>
                     dplyr::summarize(nb_total_people=dplyr::n())) |>
  dplyr::mutate(percent = nb_people/nb_total_people)

## 8: get the plot for the data above ----
data_summarized |>
  ggplot(aes(x = factor(edition), y = percent, fill = age_class)) +
  geom_bar(stat = "identity", position = "dodge") +
  common_theme +
  xlab("Count") +
  ylab("Edition number") +
  theme(legend.position = "top")

## 9: summarized data to get percentage region per edition ----

data_region <- data_longer |>
  dplyr::mutate(region = dplyr::case_when(
    is.na(departement) ~ NA,
    departement == "35" ~ "Ille-et-Vilaine",
    departement %in% c("22","49","56") ~ "Bretagne-autres",
    departement %in% c("44","49","53","50") ~ "Bretagne-limitrophe",
    TRUE ~ "France-autres"
  ))

data_summarized_departement <- data_region |>
  dplyr::filter(presence==1&!is.na(region)) |>
  dplyr::group_by(edition,region) |>
  dplyr::summarize(nb_people=dplyr::n()) |>
  dplyr::left_join(data_longer |>
                     dplyr::filter(presence==1&!is.na(departement)) |>
                     dplyr::group_by(edition) |>
                     dplyr::summarize(nb_total_people=dplyr::n())) |>
  dplyr::mutate(percent = nb_people/nb_total_people)

## 10: get the plot for the data above ----
data_summarized_departement |>
  ggplot(aes(x = factor(edition), y = percent, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  common_theme +
  xlab("Count") +
  ylab("Edition number") +
  theme(legend.position = "top")

## 11: linear model to understand if age is different between edition and region

data_only_presence <- data_region |>
  dplyr::filter(presence==1)

model_age <- stats::lm(data=data_only_presence, age_today ~ edition+region)
car::Anova(model_age)
summary(model_age) 
