#' Simple feature ggplot wrapper
#'
#' @param object data frame
#' @param title text string
#' @param legend.position position of legend (default "bottom")
#'
#' @return gg plot object
#' @export
#' @importFrom ggplot2 aes element_text geom_sf geom_sf_label ggplot labs theme
#'             theme_minimal xlab ylab
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang .data
ggplot_sf <- function(object, title, legend.position = "bottom") {
  if(is.null(object)) return(ggplot2::ggplot())
  
  # Minimal theme, bottom legend, add title.
  ggplot2::ggplot(object) +
    ggplot2::labs(title = title) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = legend.position) +
    ggplot2::scale_fill_manual(colors) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Territory"))
}
#' Simple feature ggplot geom_sf layer
#'
#' @param p gg plot object
#' @param color edge color
#' @param fill fill color
#' @param linewidth edge line width
#' @param ... additional parameters
#'
#' @return gg plot object
#' @export
#' @rdname ggplot_sf
ggplot_layer_sf <- function(object, p = ggplot2::ggplot(), 
                            color = "black", fill = "transparent",
                            linewidth = 0.5,
                            ...) {
  p +
    ggplot2::geom_sf(data = object,
                     color = color, fill = fill, linewidth = linewidth,
                     inherit.aes = FALSE, ...) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.5, hjust=1))
}
#' Simple feature ggplot geom_text_reple label layer
#'
#' @param p gg plot object
#' @param label column with label
#' @param ... additional parameters
#'
#' @return gg plot object
#' @export
#' @rdname ggplot_sf
ggplot_layer_name <- function(object, p = ggplot2::ggplot(),
                              label = "NAME", color = "black", ...) {
  p +
    ggrepel::geom_text_repel(
#    ggplot2::geom_sf_label(
      data = object,
      ggplot2::aes(label = .data[[label]], geometry = .data$geometry,
                   size = 10),
      stat =  "sf_coordinates",
      size = 2, color = color, inherit.aes = FALSE)
}
