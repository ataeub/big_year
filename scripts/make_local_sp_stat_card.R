#' Make species coverage/stat cards
#'
#' @param observed Character vector of species you observed
#' @param available Character vector of all species possible in the locations
#' @return HTML string with a card showing observed/available species
make_species_coverage_card <- function(obs) {
  available <- make_possible_species_list(obs)
  observed <- obs$name_sc
  # Ensure unique species
  observed <- unique(observed)
  available <- unique(available)
  
  total_available <- length(available)
  total_observed <- sum(observed %in% available)
  
  # handle case with no available species
  if (total_available == 0) {
    return("<div class='stat-card'>
              <div class='author'>Species coverage</div>
              <div class='value'>0/0</div>
              <div class='label'>species</div>
            </div>")
  }
  
  paste0(
    "<div class='stat-card' style='margin:5px; display:inline-block; padding:10px; border-radius:6px; background:#f2f2f2;'>",
      "<div class='author'>Species coverage</div>",
      "<div class='value'>", total_observed, "/", total_available, "</div>",
      "<div class='label'>species</div>",
    "</div>"
  )
}
