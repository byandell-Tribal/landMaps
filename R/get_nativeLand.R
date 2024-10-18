#' Get Native Land information from native-land.ca
#'
#' @param categories one or more categories
#' @param name name to look up (exact unless `slug` provided)
#' @param key API key for `native-land.ca`
#' @param slug lookup table for slug name
#'
#' @return simple feature using `sf` package
#' @export
#' @importFrom dplyr filter mutate
#' @importFrom httr content GET
#' @importFrom stringr str_replace
#' @importFrom sf st_as_sf st_crs
get_nativeLand <- function(
    categories = c("territories","languages","treaties"),
    name = NULL,
    key = NULL,
    slug = NULL) {
  
  # Lookup `name` from `slug`.
  # Otherwise `name` must be exact match.
  if(!is.null(slug)) {
    name <- dplyr::filter(
      slug,
      grepl(paste(name, collapse = "|"), .data$Slug),
      grepl(paste(categories, collapse = "|"), .data$category))$Slug
  }
  if(length(categories) != length(name)) {
    if(length(categories) == 1) categories <- rep(categories, length(name))
  }
  out <- list()
  for(i in seq_along(name)) {
    out[[i]] <- features_reform(
      httr::content(
        httr::GET( 
          paste0("https://native-land.ca/api/index.php?maps=",
                 paste(categories[i], collapse = ","),
                 "&name=", name[i], # This is the Slug
                 "&key=", key)),
        "parsed"))
  }
  if(!length(out)) return(NULL)
  
  out <- sf::st_as_sf(
    dplyr::mutate(
      dplyr::bind_rows(out),
      category = categories))
  sf::st_crs(out) <- 4326
  out
}
