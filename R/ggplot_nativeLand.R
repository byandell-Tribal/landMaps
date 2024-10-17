#' ggplot of nativeLand data
#'
#' @param object simple features object
#' @param p initial gg plot object
#' @param colors vector of colors
#' @param title character string
#' @param label include labels if `TRUE`
#' @param wrap facet wrap if `TRUE`
#'
#' @return gg object
#' @export
#' @importFrom ggplot2 element_text facet_wrap theme
#' @importFrom rlang .data
ggplot_nativeLand <- function(object, p = ggplot_sf(object, title),
                              colors = NULL, title = title_names,
                              label = TRUE, wrap = FALSE) {
  if(is.null(object)) return(ggplot2::ggplot())
  
  if(is.null(colors)) {
    colors <- object$color
    if(is.null(colors))
      colors <- seq_len(nrow(object))
  }
  categories <- c(territories = "Territory",
                  languages = "Language",
                  treaties = "Treaty")
  if("category" %in% names(object))
    scale_names <- paste(object$Name, categories[object$category])
  else
    scale_names <- object$Name
  object$Name <- scale_names
  
  title_names <- paste(scale_names, collapse = ", ")
  names(colors) <- scale_names
  
  for(i in seq_len(nrow(object))) {
    p <- ggplot_layer_sf(
      object[i,], p, color = colors[i], alpha = 0.25, linewidth = 1)
  }
  if(label) {
    p <- ggplot_layer_name(object, p, label = "Name")
  }
  if(wrap) {
    p <- p + 
      ggplot2::facet_wrap(~ .data$category) +
      ggplot2::theme(strip.text.x = ggplot2::element_text(size = 10))
  }
  p
}