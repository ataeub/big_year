#' Make interactive table of unique species per user
#'
#' @param obs data.frame with columns: author, name_sc, name_de
#' @return DT::datatable object
make_species_html_table <- function(obs) {
  library(dplyr)
  library(DT)
  
  obs_species <- obs %>%
    group_by(author, name_sc, name_de) %>%
    summarise(n_obs = n(), .groups = "drop") %>%
    select(author, name_sc, name_de, n_obs)
  
  DT::datatable(
    obs_species,
    escape = FALSE,
    rownames = FALSE,
    options = list(pageLength = 10, scrollX = TRUE)
  )
}
