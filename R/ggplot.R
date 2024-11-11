#' Shiny Server for ggplot Example
#'
#' @param id shiny identifier
#' @param places reactive object with places
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname ggplot
#' @importFrom shiny checkboxInput column fluidPage fluidRow mainPanel
#'             moduleServer NS plotOutput radioButtons reactive renderPlot
#'             sliderInput sidebarPanel titlePanel uiOutput
#' @importFrom ggspatial annotation_map_tile
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom dplyr bind_rows
ggplotServer <- function(id, places = shiny::reactive(NULL)) {
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
          # Base map from OpenStreetMap
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
      shiny::req(input$height)
      shiny::plotOutput(ns("main_plot"), height = paste0(input$height, "px"))
    })
  })
}
#' Shiny Module Input for ggplot
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname ggplot
#' @export
ggplotInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sliderInput(ns("height"), "Height:", 300, 800, 500, 100),
    shiny::fluidRow(
      shiny::column(4, shiny::checkboxInput(ns("osm"), "Geo Layer?", TRUE)),
      shiny::column(8, shiny::uiOutput(ns("zoomin"))),
    )
  )
}
#' Shiny Module Output for ggplot
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname ggplot
#' @export
ggplotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("show_plot"))
}
#' Shiny Module App for ggplot
#' @return nothing returned
#' @rdname ggplot
#' @export
ggplotApp <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Land Shapefiles"),
    shiny::sidebarPanel(
      censusInput("census"),
      nativeLandInput("nativeLand"),
      ggplotInput("ggplot")
    ),
    shiny::mainPanel(
      ggplotOutput("ggplot")
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

    ggplotServer("ggplot", places)
  }
  shiny::shinyApp(ui, server)
}