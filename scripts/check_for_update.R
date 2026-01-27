#' Check for new iNaturalist observations
#'
#' Exits the R process with status 78 (graceful exit for GitHub Actions)
#' if no new observations are found since the last recorded date.
#'
#' @param users Character vector of iNaturalist usernames.
#' @param taxon_id Numeric taxon ID.
#' @param last_obs_file Path to file storing last processed observation date.
#' @param per_page Observations per API request (default 1, latest only).
check_inat_updates <- function(users,
                               taxon_id,
                               last_obs_file = "docs/last_obs_date.txt",
                               per_page = 1) {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Please install httr and jsonlite first.")
  }

  latest_dates <- sapply(users, function(u) {
    r <- httr::GET(
      "https://api.inaturalist.org/v1/observations",
      query = list(
        user_id = u,
        taxon_id = taxon_id,
        per_page = per_page,
        order_by = "observed_on",
        order = "desc"
      )
    )
    httr::stop_for_status(r)
    d <- jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), flatten = TRUE)
    if (length(d$results) == 0) return(NA) 
    as.POSIXct(d$results[[1]]$time_observed_at, tz = "UTC")
  })

  latest_date <- max(latest_dates, na.rm = TRUE)

  if (file.exists(last_obs_file)) {
    last_date <- readLines(last_obs_file) |> as.POSIXct(tz = "UTC")
    if (latest_date <= last_date) {
      message("No new observations since last build. Exiting workflow.")
      quit(save = "no", status = 78) # GitHub Actions graceful exit
    }
  }

  # Save the new latest date
  dir.create(dirname(last_obs_file), showWarnings = FALSE, recursive = TRUE)
  writeLines(as.character(latest_date), last_obs_file)
  message("New observations found. Continuing workflow...")
}
check_inat_updates("kjaeck,splendidfairywren", 3)
