#' Shiny Server for landTable Display
#'
#' @param id shiny identifier
#' @param places reactive object with places
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname landTable
#' @importFrom shiny fluidPage mainPanel moduleServer NS reactive req
#'             sidebarPanel titlePanel
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom dplyr arrange
#' @importFrom rlang .data
landTableServer <- function(id, places = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$places <- DT::renderDataTable({
        shiny::req(places())
        table_setup(places())
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
  census_geometry <- readRDS("data/census_geometry.rds")
  
  ui <- shiny::fluidPage(
    shiny::titlePanel("Land Maps"),
    shiny::sidebarPanel(
      censusInput("census")
    ),
    shiny::mainPanel(
      landTableOutput("landTable")
    )
  ) 
  server <- function(input, output, session) {
    census_places <- censusServer("census", census_geometry)
    landTableServer("landTable", census_places)
  }
  shiny::shinyApp(ui, server)
}