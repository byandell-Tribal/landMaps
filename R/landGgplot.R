#' Shiny Server for landGgplot Example
#'
#' @param id shiny identifier
#' @param mainpar reactiveValue with height
#' @param places reactive object with places
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname landGgplot
#' @importFrom shiny checkboxInput column fluidPage fluidRow mainPanel
#'             moduleServer NS plotOutput radioButtons reactive renderPlot
#'             sliderInput sidebarPanel titlePanel uiOutput
#' @importFrom ggspatial annotation_map_tile
landGgplotServer <- function(id, mainpar, places = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Zoomin for annotation map tile
    output$zoomin <- shiny::renderUI({
      if(shiny::isTruthy(input$osm)) {
        selected <- "1"
        if(shiny::isTruthy(input$zoomin))
          selected <- input$zoomin
        shiny::radioButtons(ns("zoomin"), "", selected = selected,
                            inline = TRUE,
                            choiceNames = c("-","o","+"),
                            choiceValues = -1:1)
      }
    })

    # Output Main Plot
    output$main_plot <- shiny::renderPlot({
      if(shiny::isTruthy(places()) && nrow(places())) {
        out <- ggplot_sf(color = unique(places()$color))
        if(shiny::isTruthy(input$zoomin) & shiny::isTruthy(input$osm)) {
          # Base map from OpenStreetMap. 
          # For other map types, see https://paleolimbot.github.io/rosm.
          out <- out +
            ggspatial::annotation_map_tile(type = "osm",
                                           zoomin = as.numeric(input$zoomin),
                                           progress = "none")
        }
        print(out + ggplot_layer_sf(places()))
      } else {
        NULL
      }
    })
    output$show_plot <- shiny::renderUI({
      shiny::req(mainpar$height)
      shiny::plotOutput(ns("main_plot"), height = paste0(mainpar$height, "px"))
    })
  })
}
#' Shiny Module Input for landGgplot
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname landGgplot
#' @export
landGgplotInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(4, shiny::checkboxInput(ns("osm"), "Geo Layer?", TRUE)),
    shiny::column(8, shiny::uiOutput(ns("zoomin"))),
  )
}
#' Shiny Module Output for landGgplot
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname landGgplot
#' @export
landGgplotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("show_plot"))
}
#' Shiny Module App for landGgplot
#' @return nothing returned
#' @rdname landGgplot
#' @export
landGgplotApp <- function() {
  census_geometry <- readRDS("data/census_geometry.rds")
  
  ui <- shiny::fluidPage(
    shiny::titlePanel("Land Maps"),
    shiny::sidebarPanel(
      censusInput("census"),
      shiny::sliderInput("height", "Height:", 300, 800, 500, 100),
      landGgplotInput("landGgplot")
    ),
    shiny::mainPanel(
      landGgplotOutput("landGgplot")
    )
  ) 
  server <- function(input, output, session) {
    census_places <- censusServer("census", census_geometry)
    landGgplotServer("landGgplot", input, census_places)
  }
  shiny::shinyApp(ui, server)
}