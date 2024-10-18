#' Read census geometry
#'
#' @param geography character string 
#'
#' @return simple feature object
#' @export
#' @importFrom tidycensus get_acs
#' @importFrom dplyr rename
#' @importFrom sf st_as_sf st_transform
#' @importFrom tibble as_tibble
read_census <- function(geography) {
  sf::st_transform(
    dplyr::select(
      dplyr::rename(
        sf::st_as_sf(
          tibble::as_tibble(
            tidycensus::get_acs(
              geography = geography,
              variables = "B02001_001",
              geometry = TRUE))),
        pop = "estimate"),
      -variable),
    crs = 4326)
}