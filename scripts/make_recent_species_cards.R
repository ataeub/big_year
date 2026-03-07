make_recent_species_cards <- function(obs) {
  recent_obs <- obs |>
    dplyr::arrange(dplyr::desc(datetime)) |>
    dplyr::group_by(author) |>
    dplyr::distinct(name_sc, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(datetime)) |>
    dplyr::ungroup()

  # Group by author and create slideshow cards
  recent_obs_cards <- recent_obs |>
    dplyr::group_by(author) |>
    dplyr::summarise(
      photos_medium = list(photo_medium),
      species = list(name_sc),
      common_names = list(name_de),
      dates = list(datetime),
      places = list(place),
      .groups = "drop"
    ) |>
    purrr::pmap_chr(
      function(author, photos_medium, species, common_names, dates, places) {
        card_id <- paste0("card_", gsub("[^a-zA-Z0-9]", "_", author))
        photos_json <- jsonlite::toJSON(photos_medium)
        species_json <- jsonlite::toJSON(species)
        common_names_json <- jsonlite::toJSON(common_names)
        dates_json <- jsonlite::toJSON(as.character(dates))
        places_json <- jsonlite::toJSON(places)
        
        paste0(
          "<div class='stat-card' id='", card_id, "' style='margin:5px; display:inline-block; padding:10px; border-radius:6px; background:#e0f7fa; width:320px;'>",
          "<div class='author' style='font-weight:bold; margin-bottom:5px;'>", author, "</div>",
          "<div class='slideshow-container' style='position:relative; width:300px; margin:0 auto;'>",
          "<img class='slideshow-image' src='' style='width:300px; height:300px; object-fit:cover; border-radius:4px;' />",
          "<div class='photo-counter' style='text-align:center; font-size:14px; margin-top:8px; color:#333; line-height:1.6;'></div>",
          if (length(photos_medium) > 1) {
            paste0(
              "<button class='next-btn' style='position:absolute; left:0; top:130px; background:rgba(0,0,0,0.5); color:white; border:none; padding:5px 10px; cursor:pointer; border-radius:3px;'>❮</button>",
              "<button class='prev-btn' style='position:absolute; right:0; top:130px; background:rgba(0,0,0,0.5); color:white; border:none; padding:5px 10px; cursor:pointer; border-radius:3px;'>❯</button>"
            )
          } else "",
          "</div>",
          "<script>",
          "(function() {",
          "  const photos = ", photos_json, ";",
          "  const species = ", species_json, ";",
          "  const commonNames = ", common_names_json, ";",
          "  const dates = ", dates_json, ";",
          "  const places = ", places_json, ";",
          "  let currentIndex = 0;",
          "  const card = document.getElementById('", card_id, "');",
          "  const img = card.querySelector('.slideshow-image');",
          "  const counter = card.querySelector('.photo-counter');",
          "  const formatDate = (dateStr) => {",
          "    const d = new Date(dateStr);",
          "    const day = d.getDate();",
          "    const month = d.toLocaleString('en-US', { month: 'short' });",
          "    const year = d.getFullYear();",
          "    const suffix = (day % 10 === 1 && day !== 11) ? 'st' : (day % 10 === 2 && day !== 12) ? 'nd' : (day % 10 === 3 && day !== 13) ? 'rd' : 'th';",
          "    return day + suffix + ' ' + month + '. ' + year;",
          "  };",
          "  const updateImage = () => {",
          "    img.src = photos[currentIndex];",
          "    const place = places[currentIndex] && places[currentIndex] !== 'null' ? places[currentIndex] : 'hidden location';",
          "    counter.innerHTML = (photos.length - currentIndex) + '/' + photos.length + '<br><b><i>' + species[currentIndex] + '</i></b><br>' + commonNames[currentIndex] + '<br>' + formatDate(dates[currentIndex]) + '<br>' + place;",
          "  };",
          "  updateImage();",
          "  const nextBtn = card.querySelector('.next-btn');",
          "  const prevBtn = card.querySelector('.prev-btn');",
          "  if (nextBtn) nextBtn.onclick = () => { currentIndex = (currentIndex + 1) % photos.length; updateImage(); };",
          "  if (prevBtn) prevBtn.onclick = () => { currentIndex = (currentIndex - 1 + photos.length) % photos.length; updateImage(); };",
          "})();",
          "</script>",
          "</div>"
        )
      }
    )
  paste(c(recent_obs_cards), collapse = "\n")
}
