#' Get iNaturalist observations for one or more users, taxa, and a date range
#'
#' @param users Character vector. iNaturalist usernames or user IDs.
#' @param taxon_id Numeric or character vector. iNaturalist taxon ID(s).
#' @param start_date, end_date Character. Start and end dates in "YYYY-MM-DD" format.
#' @param lat, lng Numeric. Latitude and longitude for spatial filtering (must be used with radius_km).
#' @param radius_km Numeric. Radius in kilometers for spatial filtering.
#' @param locale_id Character. Preferred place ID for results (default "7207").
#' @param per_page Numeric. Observations per page (max 200, default 200).
#' @param page Numeric. Page number to fetch (if NULL, fetches all).
#' @return A tibble of observations, including a `photo_url` column and all metadata returned by the API.
#' @details Either `year` or `start_date`/`end_date` must be supplied, not both. If spatial filtering is used, all of `lat`, `lng`, and `radius_km` must be provided.
#' @export
get_inat_obs <- function(users,
                         taxon_id,
                         start_date = NULL,
                         end_date = NULL,
                         lat = NULL,
                         lng = NULL,
                         radius_km = NULL,
                         locale_id = "7207",
                         per_page = 200,
                         page = NULL) {
  base_url <- "https://api.inaturalist.org/v1/observations"

  fetch_page <- function(page = 1, results = TRUE) {
    r <- httr::GET(
      url = base_url,
      query = list(
        user_id = users,
        taxon_id = taxon_id,
        d1 = start_date,
        d2 = end_date,
        per_page = per_page,
        page = page,
        preferred_place_id = locale_id,
        lat = lat,
        lng = lng,
        radius = radius_km,
        order_by = "created_at" # Always order by created datetime
      )
    )
    httr::stop_for_status(r)
    txt <- httr::content(r, as = "text", encoding = "UTF-8")
    meta <- jsonlite::fromJSON(txt, flatten = TRUE)
    if (results) meta$results else meta
  }

  if (!is.null(page)) {
    results <- fetch_page(page = page, results = TRUE)
  } else {
    # First request to get total results
    dat <- fetch_page(results = FALSE)
    total_results <- dat$total_results
    total_pages <- ceiling(total_results / per_page)

    # Fetch all pages
    results <- purrr::map_dfr(seq_len(total_pages), fetch_page)
  }
  results
}
