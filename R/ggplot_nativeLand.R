#' ggplot of nativeLand data
#'
#' @param object simple features object
#' @param colors vector of colors
#' @param title character string
#'
#' @return gg object
#' @export
#' @importFrom ggplot2 geom_sf ggplot guide_legend guides labs scale_fill_manual
#'             theme theme_minimal
ggplot_nativeLand <- function(object, colors = NULL, title = title_names) {
  
  if(is.null(colors)) {
    colors <- object$color
    if(is.null(colors))
      colors <- seq_len(nrow(object))
  }
  scale_names <- paste(object$Name, object$category)
  title_names <- paste(scale_names, collapse = ", ")
  names(colors) <- scale_names
  
  p <- ggplot2::ggplot()
  for(i in seq_len(nrow(object))) {
    p <- p +
      ggplot2::geom_sf(data = object[i,], color = colors[i], alpha = 0.25,
                       linewidth = 1, fill = "transparent",
                             inherit.aes = FALSE) +
      ggplot2::geom_sf_label(data = object[i,], fill = colors[i], alpha = 0.25,
                       label = scale_names[i],
                       inherit.aes = FALSE)
  }
  p +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_manual(colors) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Territory"))
}