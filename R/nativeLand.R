#' Shiny Server for nativeLand Example
#'
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname nativeLand
#' @importFrom shiny a bootstrapPage checkboxInput moduleServer NS
#'             plotOutput renderPlot renderUI selectInput shinyApp sliderInput
#'             uiOutput
nativeLandServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # URL for
    output$password <- shiny::renderUI({
      shiny::tagList(
        "Set up key at",
        shiny::a("Native Land API", href = "https://api-docs.native-land.ca/"),
        shiny::passwordInput(ns("password"), "Enter Native Land Key")
      )
    })
    
    slug <- readRDS("data/NativeLandSlug.rds")
    slugfest <- tidyr::unite(slug, catname, sep = ", ")$catname
    output$catname <- shiny::renderUI({
      shiny::selectizeInput(ns("catname"), "Category, Name:", slugfest,
                            multiple = TRUE)
    })
    
    tribes <- shiny::reactive({
      shiny::req(input$catname, input$password)
      category <- stringr::str_remove(input$catname, ", .*$")
      name <- stringr::str_remove(input$catname, "^.*, ")
      get_nativeLand(category, name, input$password, slug)
    })
    
    # Output Main Plot
    output$main_plot <- shiny::renderPlot({
      print(ggplot_nativeLand(tribes(), wrap = TRUE))
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
    shiny::uiOutput(ns("catname"))
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