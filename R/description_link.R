#' @importFrom sf st_centroid st_coordinates st_drop_geometry
#' @importFrom rlang .data
#' @importFrom stringr str_detect
description_link <- function(object){
  if(!nrow(object)) return(NULL)
  
  # Get centroids
  # *** This breeaks for Laramie with Treaties ***.
  # *** Presumably one of the geometries is degenerate.
  # Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
  # Loop 0 is not valid: Edge 3 is degenerate (duplicate vertex)
  # *** Also breaks on Laramie if key is invalid, Not catching properly ***
  cent <- signif(sf::st_coordinates(sf::st_centroid(object)), 3)
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