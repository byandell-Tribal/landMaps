#' Shiny Server for census Example
#'
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname census
#' @importFrom shiny a checkboxInput column fluidPage mainPanel moduleServer NS
#'             plotOutput renderPlot renderUI selectInput selectizeInput
#'             shinyApp sidebarLayout sidebarPanel sliderInput tagList
#'             titlePanel uiOutput updateSelectizeInput
#' @importFrom stringr str_detect
censusServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    census_geometry <- readRDS("data/census_geometry.rds")
    cenfest <- tidyr::unite(census_geometry, catname, 
                            geography, NAME, sep = ": ")
    output$catname <- shiny::renderUI({
      shiny::selectizeInput(ns("catname"), "Select Geography, Name:",
                            NULL,
                            multiple = TRUE)
    })
    catnames <- shiny::reactive({
      shiny::req(input$category)
      cenfest$catname[stringr::str_detect(cenfest$catname,
        paste0("^", paste(input$category, collapse = "|")))]
    })
    shiny::observeEvent(
      shiny::req(catnames()), {
        shiny::updateSelectizeInput(session, "catname", choices = catnames(),
                                    server = TRUE, selected = input$catname)
      },
      ignoreNULL = FALSE, label = "update_catname")
    
    
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
    shiny::fluidRow(
      shiny::column(4, 
        shiny::selectInput(ns("category"), "Select Category(s):", 
                           c("aiannh", "states", "counties"),
                           multiple = TRUE)),
      shiny::column(8, shiny::uiOutput(ns("catname")))))
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
  
  ui <- function() {
    shiny::fluidPage(
      shiny::titlePanel("Census Units"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::tagList(
            censusInput("census"),
            "Currently adds one at a time.",
            "Could have buttons to add all counties and/or aiannh in states at once.")
        ),
        shiny::mainPanel(
          censusOutput("census"))
      )
    )
  }
  server <- function(input, output, session) {
    censusServer("census")
  }
  shiny::shinyApp(ui, server)
}