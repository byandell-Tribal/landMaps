#' Thematic Maps Wrapper
#' 
#' @param object shapefile
#' @return tmap element
#' @export
#' @importFrom tmap tm_fill tm_lines tm_shape
#' @importFrom sf st_cast
tmap_wrapper <- function(object) {
  object <- split(object, object$category)
  p <- NULL
  for(category in names(object)) {
    out <- object[[category]]
    out$category <- NULL
    p <- p + tmap_segment(out, category)
  }
  p
}
tmap_segment <- function(object, name) {
  # Cast multipolygons as multilinestring to give them `color`
  borders <- sf::st_cast(object, "MULTILINESTRING")
  tmap::tm_shape(object, paste(name, "ID")) +
    tmap::tm_fill(alpha = 0) +
    tmap::tm_shape(borders, name) +
    tmap::tm_lines(col = "color", lwd = 2)
}
# Idea for splitting. Could also consider `tm_facets`
# split
