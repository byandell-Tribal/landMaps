#' Shiny Server for census Example
#'
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname census
#' @importFrom shiny a bootstrapPage checkboxInput h2 moduleServer NS
#'             plotOutput renderPlot renderUI selectInput shinyApp sliderInput
#'             uiOutput
censusServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    census_geometry <- readRDS("data/census_geometry.rds")
    cenfest <- tidyr::unite(census_geometry, catname, 
                            geography, NAME, sep = ": ")
    output$catname <- shiny::renderUI({
      shiny::selectizeInput(ns("catname"), "Select Geography, Name:",
                            cenfest$catname,
                            multiple = TRUE)
    })
    
    places <- shiny::reactive({
      shiny::req(input$catname)
      census_geometry[which(cenfest$catname %in% input$catname),]
    })
    
    # Output Main Plot
    output$main_plot <- shiny::renderPlot({
      print(ggplot_sf() + 
              landMaps:::ggplot_layer_sf(places()))
    })
  })
}
#' Shiny Module Input for census
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname census
#' @export
censusInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h2("Census Maps"),
    shiny::uiOutput(ns("catname"))
  )
}
#' Shiny Module Output for census
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname census
#' @export
censusOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("main_plot"), height = "300px")
}
#' Shiny Module App for census
#' @return nothing returned
#' @rdname census
#' @export
censusApp <- function() {
  
  ui <- shiny::bootstrapPage(
    censusInput("census"), 
    censusOutput("census"),
  )
  server <- function(input, output, session) {
    censusServer("census")
  }
  shiny::shinyApp(ui, server)
}