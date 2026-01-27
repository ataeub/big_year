get_parent_taxon <- function(taxon_id) {
  base_url <- "https://api.inaturalist.org/v1/taxa/"

  r <- httr::GET(
    url = base_url,
    query = list(
      id = taxon_id,
      locale = "de", # German locale (Change if necessary)
      preferred_place_id = "7207" # German locale (Change if necessary)
    )
  )

  httr::stop_for_status(r)

  dat <- jsonlite::fromJSON(
    httr::content(r, as = "text", encoding = "UTF-8"),
    flatten = TRUE
  )$results

  dat |>
    dplyr::select(
      taxon.id =
        id,
      taxon.name =
        name,
      taxon.rank_level =
        rank_level,
      taxon.preferred_common_name =
        preferred_common_name,
      taxon.conservation_status.authority =
        matches("conservation_status.authority"),
      taxon.conservation_status.status_name =
        matches("conservation_status.status_name"),
      taxon.establishment_means.establishment_means =
        matches("establishment_means.establishment_means")
    )
}
