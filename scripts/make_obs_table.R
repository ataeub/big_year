make_obs_table <- function(users,
                           year,
                           taxon_id,
                           user_pseudonyms = NULL,
                           only_species = TRUE,
                           ssp_to_sp = TRUE,
                           cache = TRUE) {
  # define cache file
  cache_file <- "cache/observations.rds"

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
      year = year,
      per_page = 1,
      page = 1
    )

    # compare newest datetime in cache vs API
    latest_cache_dt <- max(as.POSIXct(cache_obs$time_observed_at, tz = "UTC"))
    latest_api_dt <- as.POSIXct(latest_inat_obs$time_observed_at[1], tz = "UTC")

    latest_cache_dt >= latest_api_dt
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
