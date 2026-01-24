plot_obs_map <- function(obs) {
  require(dplyr)
  require(tidyr)
  require(leaflet)
  require(htmltools)

  map_data <- obs %>%
    # split the single string "lat, lon" into numeric columns
    separate(
      col = coords,
      into = c("lat", "lon"),
      sep = ",\\s*",
      convert = TRUE,
      remove = FALSE
    ) %>%
    # convert datetime to POSIXct
    mutate(datetime = as.POSIXct(datetime, tz = "UTC")) %>%
    # filter only valid coordinates
    filter(
      !is.na(lat) & !is.na(lon),
      lat >= -90 & lat <= 90,
      lon >= -180 & lon <= 180
    )

  # color per author
  pal <- colorFactor(
    palette = "Set1",
    domain = map_data$author
  )

  leaflet(map_data) %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = 6,
      stroke = FALSE,
      fillOpacity = 0.8,
      color = ~ pal(author),

      # hover label
      label = ~ paste0(
        "<b>", name_sc, "</b><br/>",
        ifelse(is.na(name_de), "", name_de), "<br/>",
        author
      ) %>% lapply(htmltools::HTML),

      # popup
      popup = ~ paste0(
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
      ) %>% lapply(htmltools::HTML)
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~author,
      title = "Observer"
    )
}
