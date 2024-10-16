#' ggplot of nativeLand data
#'
#' @param object simple features object
#' @param colors vector of colors
#' @param title character string
#' @param label include labels if `TRUE`
#' @param wrap facet wrap if `TRUE`
#'
#' @return gg object
#' @export
#' @importFrom ggplot2 element_text facet_wrap geom_sf ggplot guide_legend
#'             guides labs scale_fill_manual theme theme_minimal xlab ylab
#' @importFrom rlang .data
ggplot_nativeLand <- function(object, colors = NULL, title = title_names,
                              label = TRUE, wrap = FALSE) {
  if(is.null(object)) return(NULL)
  
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
  
  title_names <- paste(scale_names, collapse = ", ")
  names(colors) <- scale_names
  
  p <- ggplot2::ggplot(object)
  for(i in seq_len(nrow(object))) {
    p <- p +
      ggplot2::geom_sf(data = object[i,], color = colors[i], alpha = 0.25,
                       linewidth = 1, fill = "transparent",
                             inherit.aes = FALSE)
    if(label) {
      p <- p +
        ggplot2::geom_sf_label(data = object[i,], fill = colors[i],
                               alpha = 0.25,
                               label = scale_names[i],
                               inherit.aes = FALSE)
      
    }
  }
  if(wrap) p <- p + ggplot2::facet_wrap(~ .data$category)
  p +
    ggplot2::labs(title = title) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.5, hjust=1)) +
    ggplot2::scale_fill_manual(colors) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Territory"))
}