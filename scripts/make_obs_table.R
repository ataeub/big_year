make_obs_table <- function(users,
                           year = NULL,
                           start_date = NULL,
                           end_date = NULL,
                           taxon_id,
                           lat = NULL,
                           lng = NULL,
                           radius_km = NULL,
                           locale_id = "7207",
                           user_pseudonyms = NULL,
                           only_species = TRUE,
                           ssp_to_sp = TRUE,
                           cache = TRUE) {
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

  if (cache) {
    # define cache file
    cache_file <- glue::glue(
      "cache/observations_{users}_{start_date}_{end_date}_{taxon_id}.rds"
    )

    # create cache directory if missing
    if (!dir.exists("cache")) dir.create("cache")

    # helper: check if cache is still valid
    is_cache_valid <- function() {
      if (!file.exists(cache_file)) {
        return(FALSE)
      }

      cache_obs <- readRDS(cache_file)

      # get newest observation from iNaturalist
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

      # compare newest datetime in cache vs API
      latest_cache_dt <- max(as.POSIXct(cache_obs$time_observed_at, tz = "UTC"))
      latest_api_dt <- as.POSIXct(latest_inat_obs$time_observed_at[1], tz = "UTC")

      latest_cache_dt >= latest_api_dt
    }
  }

  # load from cache if valid
  if (cache && is_cache_valid()) {
    obs <- readRDS(cache_file)
  } else {
    # fetch full dataset from API
    obs <- get_inat_obs(
      users = users,
      taxon_id = taxon_id,
      year = year
    )

    # save to cache
    saveRDS(obs, cache_file)
  }

  if (nrow(obs) == 0) {
    stop("No observations found for the selected filters.")
  }

  # replace subspecies with parent species if needed
  if (ssp_to_sp) obs <- replace_subspecies_with_parent(obs)

  # rename authors if pseudonyms provided
  if (!is.null(user_pseudonyms)) {
    obs <- rename_obs_authors(obs, user_pseudonyms)
  }

  obs <- obs |>
    dplyr::mutate( # Extract first photo URL for each observation
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

  if (isTRUE(ssp_to_sp)) {
    obs <- replace_subspecies_with_parent(obs)
  }

  if (!is.null(user_pseudonyms)) {
    obs <- rename_obs_authors(obs, user_pseudonyms)
  }

  obs |>
    dplyr::filter(if (only_species) taxon.rank_level <= 10 else TRUE) |>
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
}
