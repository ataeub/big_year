plot_obs_map <- function(obs) {

  map_data <- obs |>
    tidyr::separate(
      col = coords,
      into = c("lat", "lon"),
      sep = ",\\s*",
      convert = TRUE,
      remove = FALSE
    ) |>
    dplyr::mutate(
      datetime = as.POSIXct(datetime, tz = "UTC")
    ) |>
    dplyr::filter(
      !is.na(lat) & !is.na(lon),
      lat >= -90 & lat <= 90,
      lon >= -180 & lon <= 180
    )

  pal <- leaflet::colorFactor(
    palette = "Set1",
    domain = map_data$author
  )

  leaflet::leaflet(map_data) |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) |>
    leaflet::addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = 6,
      stroke = FALSE,
      fillOpacity = 0.8,
      color = ~pal(author),

      label = ~paste0(
        "<b>", name_sc, "</b><br/>",
        ifelse(is.na(name_de), "", name_de), "<br/>",
        author
      ) |> lapply(htmltools::HTML),

      popup = ~paste0(
        "<b>", name_sc, "</b>",
        ifelse(is.na(name_de), "", paste0(" (", name_de, ")")),
        "<br/><br/>",
        "<b>Observer:</b> ", author, "<br/>",
        "<b>Date:</b> ", format(datetime, "%Y-%m-%d %H:%M"), "<br/><br/>",
        ifelse(
          is.na(photo) | photo == "",
          "",
          paste0(
            "<img src='", photo,
            "' style='width:100%; max-width:250px;' ",
            "onclick=\"this.src='", photo_full, "'\">"
          )
        )
      ) |> lapply(htmltools::HTML)
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = ~author,
      title = "Observer"
    )
}
