make_species_html_table <- function(obs) {

  obs_species <- obs |>
    dplyr::distinct(author, name_sc, name_de, cons_auth, cons_stat) |>
    dplyr::group_by(name_sc, name_de) |>
    dplyr::summarise(
      observed_by = paste(sort(unique(author)), collapse = ", "),
      RL = paste(sort(unique(cons_stat[cons_auth == "Rote-Liste-Zentrum"])), collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::select(name_de, name_sc, observed_by, RL)

  DT::datatable(
    obs_species,
    escape = FALSE,
    rownames = FALSE,
    filter = "top",        # keeps column filters but static
    options = list(pageLength = 10, scrollX = TRUE)
  )
}
