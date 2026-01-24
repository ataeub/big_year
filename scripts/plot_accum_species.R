plot_accum_species <- function(obs) {

  require(ggplot2)

  plot_data <- obs |>
    # ensure datetime is ordered
    dplyr::arrange(author, name_sc, datetime) |>
    # keep only the FIRST observation per species per author
    dplyr::group_by(author, name_sc) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    # convert to day-of-year
    dplyr::mutate(day = lubridate::yday(datetime)) |>
    # count new species per day
    dplyr::count(author, day, name = "n") |>
    # complete full year per author
    tidyr::complete(
      author,
      day = 1:366,
      fill = list(n = 0)
    ) |>
    dplyr::arrange(author, day) |>
    # cumulative sum per author
    dplyr::group_by(author) |>
    dplyr::mutate(cum_n = cumsum(n)) |>
    dplyr::ungroup() |>
    # add day 0 baseline
    dplyr::bind_rows(
      obs |>
        dplyr::distinct(author) |>
        dplyr::mutate(day = 0, n = 0, cum_n = 0)
    ) |>
    dplyr::arrange(author, day)

  ggplot(plot_data, aes(x = day, y = cum_n, color = author)) +
    geom_line() +
    geom_ribbon(aes(ymin = 0, ymax = cum_n, fill = author),
            alpha = 0.2) +
    scale_x_continuous(
      limits = c(0, 366),
      breaks = seq(0, 360, 30),
      expand = c(0, 0)
    ) +
    labs(
      x = "Day of year",
      y = "Cumulative number of species",
      title = "Cumulative species observations"
    ) +
    theme_classic(base_size = 15) +
    theme(
      panel.grid.major.y = element_line(
        linewidth = 0.3,
        color = "#0000003d"
      )
    )
}