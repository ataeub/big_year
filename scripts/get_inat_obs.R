#' Get iNaturalist observations for a given user, taxon, and year
#'
#' @param user Character. iNaturalist username or user ID.
#' @param taxon_id Numeric. iNaturalist taxon ID.
#' @param year Numeric. Year of observations.
#' @param per_page Numeric. Observations per page (max 200).
#' @param verbose Logical. Print progress messages.
#' @return A data.frame of observations with a `photo_url` column.
#' @export
get_inat_obs <- function(users,
                         taxon_id,
                         year,
                         per_page = 50,
                         verbose = FALSE) {
  base_url <- "https://api.inaturalist.org/v1/observations"
  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year, "-12-31")

  # First request to get total results
  r <- httr::GET(
    url = base_url,
    query = list(
      user_id = users,
      taxon_id = taxon_id,
      d1 = start_date,
      d2 = end_date,
      per_page = per_page,
      page = 1,
      preferred_place_id = "7207" # German locale (Change if necessary)
    )
  )
  httr::stop_for_status(r)
  txt <- httr::content(r, as = "text", encoding = "UTF-8")
  dat <- jsonlite::fromJSON(txt, flatten = TRUE)

  total_results <- dat$total_results
  total_pages <- ceiling(total_results / per_page)

  if (verbose) {
    message("Total observations: ", total_results)
    message("Pages to fetch: ", total_pages)
  }

  # Helper: fetch single page
  fetch_page <- function(page) {
    r <- httr::GET(
      url = base_url,
      query = list(
        user_id = users,
        taxon_id = taxon_id,
        d1 = start_date,
        d2 = end_date,
        per_page = per_page,
        page = page,
        preferred_place_id = "7207"
      )
    )
    httr::stop_for_status(r)
    jsonlite::fromJSON(
      httr::content(r, as = "text", encoding = "UTF-8"),
      flatten = TRUE
    )$results
  }

  # Fetch all pages
  results <- purrr::map_dfr(seq_len(total_pages), fetch_page)

  # Extract first photo URL for each observation
  results <- results |>
    dplyr::mutate(
      photo_url = purrr::map_chr(photos, function(x) {
        if (is.null(x$url[1])) {
          NA_character_
        } else {
          x$url[1]
        }
      })
    ) |>
    dplyr::mutate(
      photo_url_full = stringr::str_replace(
        photo_url,
        "square.jpg",
        "original.jpg"
      )
    )
  results
}
