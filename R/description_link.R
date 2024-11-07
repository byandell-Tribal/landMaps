description_link <- function(object){
  if(!nrow(object)) return(NULL)
  
  object <- sf::st_drop_geometry(object)
  dplyr::mutate(object,
    description = ifelse(stringr::str_detect(.data$description, "^http"),
                         paste0('<a href="', .data$description,
                                '" target="_blank">',
                                basename(.data$description), '</a>'),
                         .data$description))
}