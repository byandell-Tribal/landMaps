#' ggplot of nativeLand data
#'
#' @param object simple features object
#' @param colors vector of colors
#' @param title character string
#' @param label include labels if `TRUE`
#'
#' @return gg object
#' @export
#' @importFrom ggplot2 facet_wrap
#' @importFrom rlang .data
ggplot_nativeLand <- function(object,
                              colors = NULL, title = title_names,
                              label = TRUE) {
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
  
  # p is a list of ggplot2 components.
  p <- list(
    ggplot2::labs(title = title),
    ggplot2::scale_fill_manual(colors),
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Territory")))
  
  for(i in seq_len(nrow(object))) {
    p <- c(p, ggplot_layer_sf(
      object[i,], color = colors[i], alpha = 0.25, linewidth = 1))
  }
  if(label) {
    p <- c(p, ggplot_layer_name(object, label = "Name"))
  }
  p
}