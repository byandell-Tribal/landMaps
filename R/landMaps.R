#' Shiny Server for landMaps Package
#'
#' @param id shiny identifier
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname landMaps
#' @importFrom shiny fluidPage mainPanel moduleServer NS radioButtons reactive 
#'             req sidebarPanel tagList titlePanel 
landMapsServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    census_places <- censusServer("census")
    nativeLand_places <- nativeLandServer("nativeLand")
    
    places <- shiny::reactive({
      order_places(nativeLand_places(), census_places())
    })
    
    landGgplotServer("landGgplot", input, places)
    landTmapServer("landTmap", input, places)
    landTableServer("landTable", places)
    
    output$geo <- shiny::renderUI({
      if(shiny::req(input$dynamic) == "Static") {
        landGgplotInput(ns("landGgplot"))
      }
    }
                                    )
    output$landPlot <- shiny::renderUI({
      switch(shiny::req(input$dynamic),
        Static  = landGgplotOutput(ns("landGgplot")),
        Dynamic = landTmapOutput(ns("landTmap")))
    })
    
  })
}
#' Shiny Module Input for landMaps
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname landMaps
#' @export
landMapsInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    censusInput(ns("census")),
    nativeLandInput(ns("nativeLand")),
    shiny::radioButtons(ns("dynamic"), "", c("Dynamic", "Static"),
                        inline = TRUE),
    shiny::sliderInput(ns("height"), "Height:", 300, 800, 500, 100),
    shiny::uiOutput(ns("geo"))
  )
}
#' Shiny Module Output for landMaps
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname landMaps
#' @export
landMapsOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("landPlot")),
    landTableOutput(ns("landTable")))
}
#' Shiny Module App for landMaps
#' @return nothing returned
#' @rdname landMaps
#' @export
landMapsApp <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Land Maps"),
    shiny::sidebarPanel(
      landMapsInput("landMaps")
    ),
    shiny::mainPanel(
      landMapsOutput("landMaps")
    )
  ) 
  server <- function(input, output, session) {
    landMapsServer("landMaps")
  }
  shiny::shinyApp(ui, server)
}