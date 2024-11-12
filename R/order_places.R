#' @importFrom dplyr bind_rows
order_places <- function(...) {
  out <- dplyr::bind_rows(...)
  
  # Remove possible duplication places by `Name`.
  out <- out[!duplicated(out$Name),]
  
  # Order `category` levels.
  categories <- c("territories", "languages", "states", "treaties", "aiannh")
  categories <- categories[!is.na(match(categories, out$category))]
  out$category <- factor(out$category, categories)
  
  out
}