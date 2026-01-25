#' Make interactive table of individual observations
#'
#' @param obs data.frame with columns: author, datetime, name_sc, name_de, place, photo, photo_full
#' @param show_photos logical, whether to display thumbnails
#' @return DT::datatable object
make_obs_html_table <- function(
  obs,
  show_photos = TRUE,
  only_species = TRUE) {
  library(dplyr)
  library(DT)
  
  obs_individual <- obs %>%
    mutate(
      photo = ifelse(
        show_photos & !is.na(photo) & photo != "",
        paste0("<a href='", photo_full, "' target='_blank'>",
               "<img src='", photo, "' height='60'></a>"),
        NA_character_
      )
    ) %>%
    select(author, datetime, name_de, name_sc, place, photo)
  
  DT::datatable(
    obs_individual,
    escape = FALSE,
    rownames = FALSE,
    options = list(pageLength = 10, scrollX = TRUE)
  )
}
