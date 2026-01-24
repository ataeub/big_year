make_obs_table <- function(users, year, taxon_id) {
  source("scripts/get_inat_obs.R")
  obs <- get_inat_obs(
    user = users,
    taxon_id = taxon_id,
    year = year
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
      uri
    )
  obs
}
