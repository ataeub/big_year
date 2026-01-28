rename_obs_authors <- function(obs, pseudonyms) {
  inv_map <- setNames(names(pseudonyms), unname(pseudonyms))

  obs |>
    dplyr::mutate(
      user.login_exact = dplyr::coalesce(
        inv_map[user.login_exact], 
        user.login_exact)
    )
}
