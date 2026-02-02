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
                         year = NULL,
                         start_date = NULL,
                         end_date = NULL,
                         lat = NULL,
                         lng = NULL,
                         radius_km = NULL,
                         locale_id = "7207",
                         per_page = 200,
                         page = NULL) {
  base_url <- "https://api.inaturalist.org/v1/observations"

  if (!is.null(year) && (!is.null(start_date) || !is.null(end_date))) {
    stop("Use either `year` OR `start_date`/`end_date`, not both.")
  }

  if (!is.null(lat) || !is.null(lng) || !is.null(radius_km)) {
    if (any(is.null(c(lat, lng, radius_km)))) {
      stop("lat, lng, and radius_km must all be supplied together")
    }
  }

  if (!is.null(year)) {
    start_date <- paste0(year, "-01-01")
    end_date <- paste0(year, "-12-31")
  }

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
