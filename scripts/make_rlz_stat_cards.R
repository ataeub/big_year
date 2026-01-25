make_rlz_stat_cards <- function(obs) {

  rlz <- obs |>
    dplyr::filter(cons_auth == "Rote-Liste-Zentrum") |>
    dplyr::distinct(name_sc, cons_stat) |>
    dplyr::count(cons_stat, name = "species") |>
    dplyr::arrange(dplyr::desc(species))

  # handle case where no RLZ data exists
  if (nrow(rlz) == 0) {
    return("<div class='stat-card'>
              <div class='author'>Rote Liste</div>
              <div class='value'>0</div>
              <div class='label'>species</div>
            </div>")
  }

  paste(
    purrr::pmap_chr(
      rlz,
      function(cons_stat, species) {
        paste0(
          "<div class='stat-card' style='margin:5px; display:inline-block; padding:10px; border-radius:6px; background:#f2f2f2;'>",
          "<div class='author'>", cons_stat, "</div>",
          "<div class='value'>", species, "</div>",
          "<div class='label'>species</div>",
          "</div>"
        )
      }
    ),
    collapse = "\n"
  )
}
