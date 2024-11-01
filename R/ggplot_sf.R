#' Simple feature ggplot wrapper
#'
#' @param legend.position position of legend (default "bottom")
#'
#' @return gg plot object
#' @export
#' @importFrom ggplot2 aes element_text geom_sf geom_sf_label ggplot labs
#'             scale_color_manual theme theme_minimal xlab ylab
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang .data
ggplot_sf <- function(legend.position = "none") {
  # Minimal theme, bottom legend.
  ggplot2::ggplot() +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = legend.position) +
    ggplot2::theme(strip.text.x = ggplot2::element_text(size = 10))
}
#' Simple feature ggplot geom_sf layer
#'
#' @param color edge color
#' @param fill fill color
#' @param linewidth edge line width
#' @param ... additional parameters
#'
#' @return gg plot object
#' @export
#' @rdname ggplot_sf
ggplot_layer_sf <- function(object, 
                            color = "black", fill = "transparent",
                            linewidth = 0.5,
                            ...) {
  # Set up colors. See ggplot_nativeLand for another approach if legend desired.
  if(is.null(object$color))
    object$color <- color
  colors <- unique(object$color)
  names(colors) <- colors
  
  # List of ggplot2 objects for `+` addition.
  list(
    ggplot2::geom_sf(
      data = object,
      ggplot2::aes(color = .data$color),
      fill = fill,
      linewidth = linewidth,
      inherit.aes = FALSE, ...),
    ggplot2::scale_color_manual(values = colors),
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.5, hjust=1)))
}
#' Simple feature ggplot geom_text_reple label layer
#'
#' @param label column with label
#' @param ... additional parameters
#'
#' @return gg plot object
#' @export
#' @rdname ggplot_sf
ggplot_layer_name <- function(object,
                              label = "NAME", color = "black", ...) {
  ggrepel::geom_text_repel(
#    ggplot2::geom_sf_label(
    data = object,
    ggplot2::aes(label = .data[[label]], geometry = .data$geometry,
                 size = 10),
    stat =  "sf_coordinates",
    size = 2, color = color, inherit.aes = FALSE)
}
