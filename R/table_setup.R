#' @importFrom sf st_centroid st_coordinates st_drop_geometry st_geometry
#'             st_make_valid
#' @importFrom rlang .data
#' @importFrom stringr str_detect
table_setup <- function(object){
  if(!nrow(object)) return(NULL)

  # Arrange by `category`.  
  object <- dplyr::arrange(object, .data$category)
  
  # Get centroids. Have to first make geometry valid.
  cent <- signif(sf::st_coordinates(sf::st_centroid(sf::st_make_valid(
    sf::st_geometry(object)))), 3)

  object <- sf::st_drop_geometry(object)
  dplyr::mutate(object,
    long = cent[,1], lat = cent[,2],
    description = ifelse(stringr::str_detect(.data$description, "^http"),
                         paste0('<a href="', .data$description,
                                '" target="_blank">',
                                census_basename(.data$description), '</a>'),
                         .data$description),
    color = sprintf("<span style='color: %s;'>%s</span>", .data$color, .data$color))
}