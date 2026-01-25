make_obs_table <- function(users, year, taxon_id, only_species = TRUE) {
  source("scripts/get_inat_obs.R")
  obs <- get_inat_obs(
    user = users,
    taxon_id = taxon_id,
    year = year
  ) |>
    dplyr::filter(if (only_species) taxon.rank_level <= 10) |>
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
  obs
}
