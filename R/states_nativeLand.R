#' States that overlap Selected Native Land
#'
#' @param census_sf simple feature of census data
#' @param nativeLand_sf simple feature of nativeLand data
#' @param catname selected `catname` = `category, Slug`
#' @param overlap overlap states beyond boundary if `TRUE`
#'
#' @return simple feature
#' @export
#' @importFrom dplyr bind_rows filter select
#' @importFrom sf st_agr st_geometry st_intersection st_sf st_union
#' @importFrom rlang .data
states_nativeLand <- function(census_sf, nativeLand_sf, 
                              catname = NULL, overlap = TRUE) {
  if(is.null(catname) | is.null(census_sf) | is.null(nativeLand_sf))
    return(NULL)
  
  # Filter only `states`.
  census_sf <- dplyr::filter(census_sf, .data$category == "states")
  
  # Filter `nativeLand_sf` by `catname` = `category, Slug`
  nativeLand_sf <- dplyr::select(
    dplyr::filter(
      tidyr::unite(
        nativeLand_sf,
        catname, category, Slug, sep = ", ", remove = FALSE),
      .data$catname %in% catname),
    -catname, -color)

  # Reduce `census_sf` by intersection with `nativeLand_sf`.
  out <- intersect_sf(census_sf, nativeLand_sf)
  if(overlap) {
    out <- dplyr::filter(census_sf, .data$Name %in% out$Name)
  }
  out
}
