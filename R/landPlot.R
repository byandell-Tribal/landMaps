#' Shiny Server for landPlot Example
#'
#' @param id shiny identifier
#' @param gg_object,color reactive objects
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname landPlot
#' @importFrom shiny fluidPage mainPanel moduleServer NS plotOutput reactive
#'             renderPlot sliderInput sidebarPanel titlePanel uiOutput
landPlotServer <- function(id, gg_object = shiny::reactive(NULL),
                           color = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Output Main Plot
    output$main_plot <- shiny::renderPlot({
      out <- ggplot_sf(color = color())
      if(shiny::isTruthy(gg_object())) {
        out <- out + gg_object()
      }
      print(out)
    })
    output$show_plot <- shiny::renderUI({
      shiny::req(input$height)
      shiny::plotOutput(ns("main_plot"), height = paste0(input$height, "px"))
    })
  })
}
#' Shiny Module Input for landPlot
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname landPlot
#' @export
landPlotInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::sliderInput(ns("height"), "Height:", 300, 800, 500, 100)
}
#' Shiny Module Output for landPlot
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname landPlot
#' @export
landPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("show_plot"))
}
#' Shiny Module App for landPlot
#' @return nothing returned
#' @rdname landPlot
#' @export
landPlotApp <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Land Shapefiles"),
    shiny::sidebarPanel(
      censusInput("census"),
      nativeLandInput("nativeLand"),
      landPlotInput("landPlot")
    ),
    shiny::mainPanel(
      landPlotOutput("landPlot")
    )
  ) 
  server <- function(input, output, session) {
    gg_census <- censusServer("census")
    gg_nativeLand <- nativeLandServer("nativeLand")
    # Better to do this with `...`
    gg_object <- shiny::reactive({
      list(gg_census()$gg_plot, gg_nativeLand()$gg_plot)
    })
    color <- shiny::reactive({
      unique(c(gg_census()$color, gg_nativeLand()$color))
    })
    landPlotServer("landPlot", gg_object, color)
  }
  shiny::shinyApp(ui, server)
}