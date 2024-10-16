#' Shiny Server for nativeLand Example
#'
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname nativeLand
#' @importFrom shiny bootstrapPage checkboxInput moduleServer NS plotOutput
#'             renderPlot renderUI selectInput shinyApp sliderInput uiOutput
#' @importFrom graphics hist lines rug
#' @importFrom stats density
nativeLandServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # URL for
    output$password <- shiny::renderUI({
      url <- shiny::a("Native Land API", 
                      href = "https://api-docs.native-land.ca/")
      shiny::tagList(
        shiny::renderText(paste("Set up key at", url)),
        shiny::passwordInput(ns("password"), "Enter Native Land Key")
      )
    })
    
    slug <- readRDS("data/NativeLandSlug.rds")
    output$tribe <- shiny::renderUI({
      shiny::selectInput(ns("tribe"), "Tribe", c("oceti","lakota","menominee"))
    })
    
    tribes <- shiny::reactive({
      shiny::req(input$category, input$tribe, input$password)
      get_nativeLand(input$category, input$tribe, input$password,
                     slug)
    })
    
    # Output Main Plot
    output$main_plot <- shiny::renderPlot({
      print(ggplot_nativeLand(tribes()))
    })
  })
}
#' Shiny Module Input for nativeLand
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname nativeLand
#' @export
nativeLandInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("password")),
    shiny::selectInput(ns("category"), "Category",
                       c("territories","languages","treaties")),
    shiny::uiOutput(ns("tribe"))
  )
}
#' Shiny Module Output for nativeLand
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname nativeLand
#' @export
nativeLandOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("main_plot"), height = "300px")
}
#' Shiny Module App for nativeLand
#' @return nothing returned
#' @rdname nativeLand
#' @export
nativeLandApp <- function() {
  
  ui <- shiny::bootstrapPage(
    nativeLandInput("nativeLand"), 
    nativeLandOutput("nativeLand"),
  )
  server <- function(input, output, session) {
    nativeLandServer("nativeLand")
  }
  shiny::shinyApp(ui, server)
}