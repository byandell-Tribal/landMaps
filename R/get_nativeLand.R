get_nativeLand <- function(
    categories = c("territories","languages","treaties"),
    name = NULL,
    key = NULL,
    slug = NULL) {
  
  # Get `name` from `slug`
  if(is.null(slug) & exists("nativeLandSlug")) {
    slug <- nativeLandSlug
  }
  if(!is.null(slug)) {
    name <- dplyr::filter(
      slug,
      grepl(paste(name, collapse = "|"), .data$Slug),
      grepl(paste(categories, collapse = "|"), .data$category))$Slug
  }
  features_reform(
    httr::content(
      httr::GET( 
        paste0("https://native-land.ca/api/index.php?maps=",
               paste(categories, collapse = ","),
               "&name=", name, # This is the Slug
               "&key=", key)),
      "parsed"))
}
