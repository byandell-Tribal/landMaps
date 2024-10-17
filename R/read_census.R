#' Read census geometry
#'
#' @param geography character string 
#'
#' @return simple feature object
#' @export
#' @importFrom tidycensus get_acs
#' @importFrom dplyr rename
#' @importFrom sf st_transform
read_census <- function(geography) {
  sf::st_transform(
    dplyr::select(
      dplyr::rename(
        tidycensus::get_acs(
          geography = geography,
          variables = "B02001_001",
          geometry = TRUE),
        pop = "estimate"),
      -variable),
    crs = 4326)
}