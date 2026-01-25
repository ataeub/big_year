make_author_stat_cards <- function(obs) {

  by_author <- obs |>
    dplyr::distinct(author, name_sc) |>
    dplyr::count(author, name = "species") |>
    dplyr::arrange(dplyr::desc(species)) |>
    dplyr::mutate(rank = dplyr::dense_rank(dplyr::desc(species)))

  total_species <- obs |>
    dplyr::distinct(name_sc) |>
    nrow()

  medal <- function(rank) {
    dplyr::case_when(
      rank == 1 ~ " 👑",
      rank == 2 ~ " 🥈",
      rank == 3 ~ " 🥉",
      TRUE ~ ""
    )
  }

  total_card <- paste0(
    "<div class='stat-card' style='margin:5px; display:inline-block; padding:10px; border-radius:6px; background:#e0f7fa;'>",
    "<div class='author'>Total</div>",
    "<div class='value'>", total_species, "</div>",
    "<div class='label'>species</div></div>"
  )

  author_cards <- purrr::pmap_chr(
    by_author,
    function(author, species, rank) {
      paste0(
        "<div class='stat-card' style='margin:5px; display:inline-block; padding:10px; border-radius:6px; background:#e0f7fa;'>",
        "<div class='author'>", author, medal(rank), "</div>",
        "<div class='value'>", species, "</div>",
        "<div class='label'>species</div></div>"
      )
    }
  )

  paste(c(total_card, author_cards), collapse = "\n")
}