#' Census URL
#'
#' @param object data frame (usually simple feature)
#'
#' @return data frame
#' @export
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect str_remove str_replace
census_url <- function(object) {
  # Create URL to Census Information as `description` column.
  dplyr::mutate(object,
    description = ifelse(.data$category == "counties", {
      states <- state.abb
      names(states) <- state.name
      state <- tolower(states[stringr::str_remove(.data$Name, "^.*, ")])
      county <- stringr::str_replace_all(
        tolower(stringr::str_remove(.data$Name, ", .*$")), " ", "-")
      paste0("https://catalog.data.gov/dataset/tiger-line-shapefile-",
             "2022",
             "-county-", county, "-", state, "-address-range-feature")
    }, ""),
    description = ifelse(.data$category == "states", {
      paste0("https://catalog.data.gov/dataset/?q=",
             stringr::str_replace_all(tolower(.data$Name), " ", "+"),
             "&sort=views_recent+desc&metadata_type=geospatial",
             "&ext_location=&ext_bbox=&ext_prev_extent=")
    }, .data$description),
    description = ifelse(.data$category == "aiannh", {
      paste0("https://www.census.gov/tribal/index.html?aianihh=", .data$GEOID)
    },
    .data$description)
  )
}
# Unpack `basename` of `description`.
census_basename <- function(x) {
  x <- basename(x)
  # Counties
  x <- ifelse(stringr::str_detect(x, "^tiger-line-shape"),
              stringr::str_replace(x, "^.*20..\\-county\\-(.*)\\-address.*$", "\\1"),
              x)
  # States
  x <- ifelse(stringr::str_detect(x, "^?"),
              stringr::str_replace(x, "^.q=(.*).sort.*$", "\\1"),
              x)
  # AIANNH
  x <- ifelse(stringr::str_detect(x, "^index.html?"),
              stringr::str_remove(x, "^index.html."),
              x)
  x
}
