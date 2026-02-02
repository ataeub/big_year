make_possible_species_list <- function(obs,
                                       range_km = 2,
                                       taxon_id = 3,
                                       time_range_yr = 5,
                                       cache_dir = "cache",
                                       cache = TRUE,
                                       filter_species = TRUE) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir)
  cache_file_coords <- file.path(cache_dir, sprintf("locations_rkm_%s.rds", range_km))
  cache_file_species <- file.path(cache_dir, sprintf("species_rkm_%s_taxon_%s.rds", range_km, taxon_id))

  round_coords <- function(coords, km) {
    factor <- 111 / km
    lat <- round(coords[1] * factor) / factor
    lon <- round(coords[2] * factor) / factor
    c(lat, lon)
  }

  locs <- obs |>
    dplyr::filter(!is.na(coords)) |>
    dplyr::mutate(
      lat = purrr::map_dbl(coords, ~ as.numeric(strsplit(.x, ",")[[1]][1])),
      lon = purrr::map_dbl(coords, ~ as.numeric(strsplit(.x, ",")[[1]][2])),
      lat_r = purrr::pmap_dbl(list(lat, lon), ~ round_coords(c(..1, ..2), range_km)[1]),
      lon_r = purrr::pmap_dbl(list(lat, lon), ~ round_coords(c(..1, ..2), range_km)[2])
    ) |>
    dplyr::select(lat_r, lon_r) |>
    dplyr::distinct()

  if (cache && file.exists(cache_file_coords)) {
    cached_locs <- readRDS(cache_file_coords)
    locs <- dplyr::bind_rows(cached_locs, locs) |> dplyr::distinct()
  }
  if (cache) saveRDS(locs, cache_file_coords)

  species_list <- list()
  start_date <- Sys.Date() - lubridate::years(time_range_yr)
  end_date <- Sys.Date()

  species_cache <- if (cache && file.exists(cache_file_species)) {
    readRDS(cache_file_species)
  } else {
    list()
  }

  for (i in seq_len(nrow(locs))) {
    loc <- locs[i, ]
    key <- paste0(loc$lat_r, "_", loc$lon_r)
    if (key %in% names(species_cache)) {
      species_list[[key]] <- species_cache[[key]]
      next
    }

    obs_loc <- get_inat_obs(
      users = NULL,
      taxon_id = taxon_id,
      start_date = as.character(start_date),
      end_date = as.character(end_date),
      lat = loc$lat_r,
      lng = loc$lon_r,
      radius_km = range_km
    )

    if (nrow(obs_loc) == 0 || !"taxon.name" %in% names(obs_loc)) {
      species <- character(0)
    } else {
      species <- unique(obs_loc$taxon.name)
    }
    species_list[[key]] <- species
    species_cache[[key]] <- species

    if (cache) saveRDS(species_cache, cache_file_species)
  }

  # Flatten to one vector
  all_species <- unique(unlist(species_list))

  # Optionally remove higher/lower taxonomies
  if (filter_species) {
    # Only keep two words (species)
    all_species <- all_species[stringr::str_count(
      all_species,
      "\\S+"
    ) == 2]
  }

  all_species
}