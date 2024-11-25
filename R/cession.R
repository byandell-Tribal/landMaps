#' Title
#'
#' @param object simple feature
#' @param treaty simple feature
#'
#' @return simple feature
#' @export
#' @importFrom dplyr filter mutate
#' @importFrom stringr str_remove
#' @importFrom rlang .data
cession_intersection <- function(object, treaty) {
  # Intersect cessions with treaty.
  intersect_sf(
    dplyr::filter(object, grepl("^Cession ", .data$Name)),
    treaty) |>
    dplyr::mutate(Name = stringr::str_remove(.data$Name, "Cession "))
}
#' Expand Cessions beyond Treaty boundary
#'
#' @param cessions simple feature
#' @param nativeLand simple feature
#'
#' @return simple feature
#' @rdname cession_intersection
#' @export
cession_expand <- function(cessions, nativeLand) {
  dplyr::filter(nativeLand, grepl("^Cession ", .data$Name)) |>
    dplyr::mutate(Name = stringr::str_remove(.data$Name, "Cession ")) |>
    dplyr::filter(.data$Name %in% cessions$Name)
}
#' Title
#'
#' @param cessions simple feature
#' @param treaty simple feature
#'
#' @return simple feature
#' @rdname cession_intersection
#' @export
cession_places <- function(cessions = NULL, treaty = NULL) {
  if(is.null(treaty)) return(cessions)
  if(is.null(cessions)) return(treaty)
  order_places(treaty, cessions) |>
    dplyr::mutate(category = factor(.data$Name, unique(.data$Name)))
}
#' Title
#'
#' @param cessions simple feature
#' @param treaty simple feature
#' @param title title of plot
#' 
#' @return ggplot object
#' @rdname cession_intersection
#' @export
ggplot_cession <- function(cessions, treaty = NULL, title = "") {
  places <- cession_places(cessions, treaty)
  ggplot_sf(color = places$color) + 
    ggspatial::annotation_map_tile(type = "osm", zoomin = -1) +
    ggplot_layer_sf(places) +
    ggplot_layer_name(places) +
    ggplot2::ggtitle(title)
  
}
#' Title
#'
#' @param cessions simple feature
#' @param treaty simple feature
#' 
#' @return thematic map
#' @rdname cession_intersection
#' @export
tmap_cession <- function(cessions, treaty = NULL) {
  places <- cession_places(cessions, treaty)
  tmap::tmap_mode("view")
  tmap_wrapper(places)
}