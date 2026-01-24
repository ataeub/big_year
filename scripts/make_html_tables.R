#' Make interactive observations table for Quarto
#'
#' Shows both individual observations and unique species per user with thumbnails.
#' @param obs data.frame with columns: author, datetime, name_sc, name_de, place, photo, photo_full
#' @return a tagList with tabset tables
#' @export
make_html_tables <- function(obs) {
  library(dplyr)
  library(DT)
  library(htmltools)

  # individual observations
  obs_individual <- obs %>%
    mutate(
      photo = ifelse(is.na(photo) | photo == "",
        "",
        paste0(
          "<a href='", photo_full, "' target='_blank'>",
          "<img src='", photo, "' height='60'></a>"
        )
      )
    ) %>%
    select(author, datetime, name_de, name_sc, place, photo)

  # unique species per user
  obs_species <- obs %>%
    group_by(author, name_sc, name_de) %>%
    summarise(n_obs = n(), .groups = "drop") %>%
    select(author, name_sc, name_de, n_obs)

  # create DT tables
  table_individual <- DT::datatable(obs_individual,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      scrollX = TRUE
    )
  )
  table_species <- DT::datatable(obs_species,
    escape = FALSE,
    rownames = FALSE,
    options = list(
      pageLength = 10,
      scrollX = TRUE
    )
  )

  # return as Quarto tabset using tagList
  htmltools::tagList(
    htmltools::tags$div(
      class = "tabset",
      htmltools::tags$div(
        class = "tab",
        htmltools::tags$h3("Individual Observations"),
        table_individual
      ),
      htmltools::tags$div(
        class = "tab",
        htmltools::tags$h3("Unique Species per User"),
        table_species
      )
    )
  )
}
