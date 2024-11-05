#' Native Land that overlap Selected States
#'
#' @param nativeLand_sf simple feature of nativeLand data
#' @param census_sf simple feature of census data
#' @param states selected state abbreviations
#' @param categories one or more categories
#'
#' @return simple feature
#' @export
#' @importFrom dplyr bind_rows filter select
#' @importFrom sf st_agr st_geometry st_intersection
#' @importFrom rlang .data
nativeLand_states <- function(nativeLand_sf, census_sf,
                              states = NULL, categories = "territories") {
  if(is.null(states) | is.null(nativeLand_sf) | is.null(census_sf))
    return(NULL)
  
  state_names <- array(state.name, dimnames = list(state.abb))
  census_sf <- dplyr::select(
    dplyr::filter(census_sf,
      .data$category == "states", .data$Name %in% state_names[states]),
    -color)
  nativeLand_sf <- dplyr::filter(nativeLand_sf,
    .data$category %in% categories)
  
  # Reduce `nativeLand_sf` by intersection with `census_sf`.
  intersect_sf(nativeLand_sf, census_sf)
}
nativeLand_us <- function(object, census_sf) {
  us <- union_us(census_sf)
  out <- NULL
  for(i in seq_len(nrow(object))) {
    ct <- tryCatch(
      sf::st_intersects(object[i,], us),
      error = function(e) NA)
    if(is.na(ct)) {
      ct <- 0
    } else {
      ct <- length(ct[[1]])
    }
    out[i] <- ct
  }
  dplyr::filter(object, out == 1)
}
union_us <- function(census_geometry) {
  us <- sf::st_union(
    dplyr::filter(census_geometry, .data$category == "states"))
  sf::st_sf(us)
}
# Reduce `result_sf` by intersection with `land_sf`.
intersect_sf <- function(result_sf, land_sf) {
  
  # Now take union over `land_sf` to reduce to one geometry.
  land_sf <- sf::st_sf(sf::st_union(land_sf))
  
  object <- list()
  options(warn = -1)
  on.exit(options(warn = 0))
  for(i in seq_len(nrow(result_sf))) {
    out <- result_sf[i,]
    geom <- tryCatch(
      sf::st_intersection(sf::st_geometry(out), sf::st_geometry(land_sf)),
      error = function(e) NA)
    
    if(any(is.na(geom)) || !length(geom) ||
       length(geom) != length(out$geometry)) {
      object[[i]] <- NA
    } else {
      sf::st_geometry(out) <- geom
      object[[i]] <- out
    }
  }
  dplyr::bind_rows(object[!is.na(object)])
}