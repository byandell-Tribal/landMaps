#' Shiny Server for landPlot Example
#'
#' @param id shiny identifier
#' @param places reactive object with places
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname landPlot
#' @importFrom shiny checkboxInput column fluidPage fluidRow mainPanel
#'             moduleServer NS plotOutput reactive renderPlot sliderInput
#'             sidebarPanel titlePanel uiOutput
#' @importFrom ggspatial annotation_map_tile
landPlotServer <- function(id, places = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Zoom for annotation map tile
    output$zoom <- shiny::renderUI({
      if(shiny::isTruthy(input$osm)) {
        value <- -1
        if(shiny::isTruthy(input$osm_zoom))
          value <- input$osm_zoom
        shiny::sliderInput(ns("osm_zoom"), "Zoom:", -2, 1, value, 1)
      }
    })

    # Output Main Plot
    output$main_plot <- shiny::renderPlot({
      if(shiny::isTruthy(places()) && nrow(places())) {
        out <- ggplot_sf(color = unique(places()$color))
        if(shiny::isTruthy(input$osm_zoom) & shiny::isTruthy(input$osm)) {
          # Base map from OpenStreetMap
          out <- out +
            ggspatial::annotation_map_tile(type = "osm", zoomin = input$osm_zoom,
                                           progress = "none")
        }
        print(out + ggplot_layer_sf(places()))
      } else {
        NULL
      }
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
  shiny::tagList(
    shiny::sliderInput(ns("height"), "Height:", 300, 800, 500, 100),
    shiny::fluidRow(
      shiny::column(4, shiny::checkboxInput(ns("osm"), "Geo Layer?", FALSE)),
      shiny::column(8, shiny::uiOutput(ns("zoom"))),
    )
  )
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
    census_places <- censusServer("census")
    nativeLand_places <- nativeLandServer("nativeLand")

    places <- shiny::reactive({
      out <- dplyr::bind_rows(census_places(), nativeLand_places())
      # Remove possible duplication places by `Name`.
      out[!duplicated(out$Name),]
    })

    landPlotServer("landPlot", places)
  }
  shiny::shinyApp(ui, server)
}