clean_data_1 <- function(x) {
  
  # x <- targets::tar_read("raw_data")
  
  data_1 <- x |> 
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
    dplyr::rename(age_edition_5 = ageaumomentdelachat5) |>
    dplyr::rename(connaissance_festival = question4Commentastuconnulefestival)
  return(data_1)
  
  
}