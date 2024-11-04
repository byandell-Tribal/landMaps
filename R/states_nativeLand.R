#' States that overlap Selected Native Land
#'
#' @param census_sf simple feature of census data
#' @param nativeLand_sf simple feature of nativeLand data
#' @param catname selected `catname` = `category, Slug`
#'
#' @return simple feature
#' @export
#' @importFrom dplyr bind_rows filter select
#' @importFrom sf st_agr st_geometry st_intersection
#' @importFrom rlang .data
states_nativeLand <- function(census_sf, nativeLand_sf, 
                              catname = NULL) {
  if(is.null(catname) | is.null(census_sf) | is.null(nativeland_sf))
    return(NULL)
  
  # Filter only `states` and drop `color`.
  census_sf <- dplyr::select(
    dplyr::filter(census_sf, .data$geography == "states"),
    -color)
  
  # Filter `nativeland_sf` by `catname` = `category, Slug`
  nativeLand_sf <- dplyr::select(
    dplyr::filter(
      tidyr::unite(
        nativeLand_sf,
        catname, category, Slug, sep = ", ", remove = FALSE),
      .data$catname %in% catname),
    -catname)

  # Reduce `census_sf` by intersection with `nativeLand_sf`.
  intersect_sf(census_sf, nativeLand_sf)
}
