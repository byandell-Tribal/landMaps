#' Shiny Server for landPlot Example
#'
#' @param id shiny identifier
#' @param gg_object reactive gg object
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname landPlot
#' @importFrom shiny bootstrapPage moduleServer NS
#'             plotOutput reactive renderPlot
landPlotServer <- function(id, gg_object = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Output Main Plot
    output$main_plot <- shiny::renderPlot({
      out <- ggplot_sf()
      if(shiny::isTruthy(gg_object())) {
        out <- out + gg_object()
      }
      print(out)
    })
  })
}
#' Shiny Module Output for landPlot
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname landPlot
#' @export
landPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("main_plot"), height = "300px")
}
#' Shiny Module App for landPlot
#' @return nothing returned
#' @rdname landPlot
#' @export
landPlotApp <- function() {
  ui <- shiny::bootstrapPage(landPlotOutput("landPlot"))
  server <- function(input, output, session) {
    landPlotServer("landPlot")
  }
  shiny::shinyApp(ui, server)
}