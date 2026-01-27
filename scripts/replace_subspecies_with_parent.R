replace_subspecies_with_parent <- function(obs) {
  subspp <- obs |>
    dplyr::filter(taxon.rank_level == 5) |>
    dplyr::distinct(taxon.parent_id)

  if (nrow(subspp) == 0) {
    return(obs)
  }

  parent_taxa <- purrr::map_dfr(
    subspp$taxon.parent_id,
    get_parent_taxon
  )

  if (!"taxon.conservation_status.authority.parent" %in% names(parent_taxa)) {
    parent_taxa <- parent_taxa |>
      dplyr::mutate(
        taxon.conservation_status.authority.parent = NA_character_,
        taxon.conservation_status.status_name.parent = NA_character_
      )
  }

  obs |>
    dplyr::left_join(
      parent_taxa,
      by = c("taxon.parent_id" = "taxon.id"),
      suffix = c("", ".parent")
    ) |>
    dplyr::mutate(
      taxon.name = dplyr::if_else(taxon.rank_level == 5,
        taxon.name.parent,
        taxon.name
      ),
      taxon.preferred_common_name =
        dplyr::if_else(taxon.rank_level == 5,
          taxon.preferred_common_name.parent,
          taxon.preferred_common_name
        ),
      taxon.conservation_status.authority =
        dplyr::if_else(taxon.rank_level == 5,
          taxon.conservation_status.authority.parent,
          taxon.conservation_status.authority
        ),
      taxon.conservation_status.status_name =
        dplyr::if_else(taxon.rank_level == 5,
          taxon.conservation_status.status_name.parent,
          taxon.conservation_status.status_name
        ),
      taxon.establishment_means.establishment_means =
        dplyr::if_else(taxon.rank_level == 5,
          taxon.establishment_means.establishment_means.parent,
          taxon.establishment_means.establishment_means
        )
    ) |>
    dplyr::select(-dplyr::ends_with(".parent"))
}
