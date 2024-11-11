#' Shiny Server for landTable Display
#'
#' @param id shiny identifier
#' @param places reactive object with places
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname landTable
#' @importFrom shiny checkboxInput column fluidPage fluidRow mainPanel
#'             moduleServer NS plotOutput radioButtons reactive renderPlot
#'             sliderInput sidebarPanel titlePanel uiOutput
#' @importFrom ggspatial annotation_map_tile
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom dplyr bind_rows
landTableServer <- function(id, places = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$places <- DT::renderDataTable({
        shiny::req(places())
        description_link(places())
      },
      escape = FALSE, options = list(scrollX = TRUE, pageLength = 5))
  })
}
#' Shiny Module Output for landTable
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname landTable
#' @export
landTableOutput <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("places"))
}
#' Shiny Module App for landTable
#' @return nothing returned
#' @rdname landTable
#' @export
landTableApp <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Land Shapefiles"),
    shiny::sidebarPanel(
      censusInput("census"),
      nativeLandInput("nativeLand")
    ),
    shiny::mainPanel(
      landTableOutput("landTable")
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

    landTableServer("landTable", places)
  }
  shiny::shinyApp(ui, server)
}