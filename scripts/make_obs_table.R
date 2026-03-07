library(rlang)

#' Create a cleaned observation table for iNaturalist users and taxa
#'
#' @param users Character. iNaturalist username(s) or user ID(s).
#' @param taxon_id Numeric. iNaturalist taxon ID.
#' @param year Numeric. Year of observations to fetch.
#' @param start_date Character. Start date in YYYY-MM-DD format.
#' @param end_date Character. End date in YYYY-MM-DD format.
#' @param lat Numeric. Latitude for geographic filtering.
#' @param lng Numeric. Longitude for geographic filtering.
#' @param radius_km Numeric. Search radius in kilometers.
#' @param locale_id Character. iNaturalist locale ID (default: "7207" for Germany).
#' @param user_pseudonyms Named character vector. Optional mapping of usernames to pseudonyms.
#' @param only_species Logical. If TRUE, only include species-level observations (default: TRUE).
#' @param ssp_to_sp Logical. If TRUE, replace subspecies with parent species (default: TRUE).
#' @param cache Logical. If TRUE, use cached data if available and valid (default: TRUE).
#'
#' @return A tibble with selected and cleaned observation columns, including author, datetime, scientific and common names, coordinates, place, photo URLs, conservation status, establishment means, and URI.
#' @details
#' - Fetches observations from iNaturalist using `get_inat_obs()`.
#' - Caches results to avoid redundant API calls (cache is keyed by query parameters).
#' - Optionally replaces subspecies with parent species and renames authors.
#' - Extracts and formats photo URLs.
#' - Filters to species-level observations if requested.
#' @export
make_obs_table <- function(users,
                           taxon_id,
                           year = NULL,
                           start_date = NULL,
                           end_date = NULL,
                           lat = NULL,
                           lng = NULL,
                           radius_km = NULL,
                           locale_id = "7207",
                           user_pseudonyms = NULL,
                           only_species = TRUE,
                           ssp_to_sp = TRUE,
                           cache = TRUE) {
  # Validate inputs
  if (length(users) == 0 || all(is.na(users))) {
    stop("At least one user must be provided.")
  }
  if (is.na(taxon_id) || !is.numeric(taxon_id)) {
    stop("taxon_id must be a valid numeric value.")
  }

  # Handle date parameters
  if (!is.null(year) && (!is.null(start_date) || !is.null(end_date))) {
    stop("Use either `year` OR `start_date`/`end_date`, not both.")
  }

  if (!is.null(lat) || !is.null(lng) || !is.null(radius_km)) {
    if (any(is.null(c(lat, lng, radius_km)))) {
      stop("lat, lng, and radius_km must all be supplied together.")
    }
  }

  if (!is.null(year)) {
    start_date <- paste0(year, "-01-01")
    end_date <- paste0(year, "-12-31")
  }

  # Prepare cache if enabled
  cache_file <- NULL
  if (cache) {
    cache_file <- .get_cache_path(
      users = users,
      taxon_id = taxon_id,
      start_date = start_date,
      end_date = end_date,
      lat = lat,
      lng = lng,
      radius_km = radius_km,
      locale_id = locale_id
    )
    fs::dir_create(fs::path_dir(cache_file), recurse = TRUE)

    if (file.exists(cache_file) && .is_cache_valid(cache_file, users, taxon_id, start_date, end_date, lat, lng, radius_km, locale_id)) {
      return(readRDS(cache_file))
    }
  }

  # Fetch observations from API
  obs <- get_inat_obs(
    users = users,
    taxon_id = taxon_id,
    start_date = start_date,
    end_date = end_date,
    lat = lat,
    lng = lng,
    radius_km = radius_km,
    locale_id = locale_id
  )

  if (nrow(obs) == 0) {
    stop("No observations found for the selected filters.")
  }

  if (ssp_to_sp) {
    obs <- replace_subspecies_with_parent(obs)
  }

  if (!is.null(user_pseudonyms)) {
    obs <- rename_obs_authors(obs, user_pseudonyms)
  }

  # Extract and clean photo URLs
  obs <- obs |>
    dplyr::mutate(
      photo_url = purrr::map_chr(photos, function(x) {
        if (is.null(x$url[1])) NA_character_ else x$url[1]
      })
    ) |>
    dplyr::mutate(
      photo_url_full = stringr::str_replace(
        photo_url,
        "square.jpg",
        "original.jpg"
      )
    )

  SPECIES_RANK_THRESHOLD <- 10L
  result <- obs |>
    dplyr::filter(
      if (only_species) taxon.rank_level <= SPECIES_RANK_THRESHOLD else TRUE
    ) |>
    dplyr::select(
      author = user.login_exact,
      datetime = time_observed_at,
      name_sc = taxon.name,
      name_de = taxon.preferred_common_name,
      coords = location,
      place = place_guess,
      photo = photo_url,
      photo_full = photo_url_full,
      cons_auth = taxon.conservation_status.authority,
      cons_stat = taxon.conservation_status.status_name,
      estab = taxon.establishment_means.establishment_means,
      uri
    )

  # Cache result if enabled
  if (!is.null(cache_file)) {
    saveRDS(result, cache_file)
  }

  result
}


#' Generate cache file path from query parameters
#'
#' Creates a hash-based cache filename from query parameters to avoid collisions.
#'
#' @param users Character vector of users
#' @param taxon_id Numeric taxon ID
#' @param start_date Character start date
#' @param end_date Character end date
#' @param lat Numeric latitude
#' @param lng Numeric longitude
#' @param radius_km Numeric search radius
#' @param locale_id Character locale ID
#'
#' @return Character path to cache file
#'
#' @keywords internal
.get_cache_path <- function(users, taxon_id, start_date, end_date,
                            lat, lng, radius_km, locale_id) {
  query_key <- paste(
    paste(sort(users), collapse = ","),
    taxon_id,
    start_date %||% "NA",
    end_date %||% "NA",
    lat %||% "NA",
    lng %||% "NA",
    radius_km %||% "NA",
    locale_id,
    sep = "|"
  )

  query_hash <- digest::digest(query_key, algo = "md5")
  file.path("cache", paste0("observations_", query_hash, ".rds"))
}

#' Check if cache is valid by comparing with latest API data
#'
#' Returns TRUE if cached observations are up-to-date with the latest API data.
#'
#' @param cache_file Character path to cache file
#' @param users Character vector of users
#' @param taxon_id Numeric taxon ID
#' @param start_date Character start date
#' @param end_date Character end date
#' @param lat Numeric latitude
#' @param lng Numeric longitude
#' @param radius_km Numeric search radius
#' @param locale_id Character locale ID
#'
#' @return Logical
#'
#' @keywords internal
.is_cache_valid <- function(cache_file, users, taxon_id, start_date, end_date,
                            lat, lng, radius_km, locale_id) {
  cache_obs <- try(readRDS(cache_file), silent = TRUE)
  if (inherits(cache_obs, "try-error")) {
    return(FALSE)
  }

  latest_inat_obs <- get_inat_obs(
    users = users,
    taxon_id = taxon_id,
    start_date = start_date,
    end_date = end_date,
    lat = lat,
    lng = lng,
    radius_km = radius_km,
    locale_id = locale_id,
    per_page = 1,
    page = 1
  )

  latest_cache_dt <- max(as.POSIXct(
    cache_obs$datetime,
    tz = "UTC"
  ))
  latest_api_dt <- as.POSIXct(
    latest_inat_obs$time_observed_at[1],
    tz = "UTC"
  )

  latest_cache_dt >= latest_api_dt
}
