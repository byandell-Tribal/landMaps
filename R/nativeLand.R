#' Shiny Server for nativeLand Example
#'
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname nativeLand
#' @importFrom shiny a checkboxInput column fluidPage fluidRow h2 isTruthy
#'             mainPanel moduleServer NS plotOutput renderPlot renderUI
#'             selectInput shinyApp sidebarPanel sliderInput titlePanel uiOutput
#' @importFrom ggplot2 facet_wrap
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
    
    # ** This is slow as it does `st_interaction` for every object. **
    # ** Better to consider a subset that is only in US? **
    nativeLandUS <- dplyr::filter(
      readRDS("data/nativeLandUS.rds"),
      category == "territories")
    census_geometry <- readRDS("data/census_geometry.rds")
    native_states <- shiny::reactive({
      if(!shiny::isTruthy(input$native_states))
        return(NULL)
      shiny::req(input$native_states, input$password)
      # Limit to territories for now.
      nativeLand_states(nativeLandUS, census_geometry, input$native_states)
    })
    output$native_states <- shiny::renderUI({
      shiny::selectInput(ns("native_states"), "Native Lands over States:", 
                         state.abb,
                         multiple = TRUE)
    })
    # Find states that overlap with selected native lands.
    state_natives <- shiny::reactive({
      if(!shiny::isTruthy(input$catname) | !shiny::isTruthy(input$catstate))
        return(NULL)
      shiny::req(input$catname, input$password, cat_places())
      # Limit to territories for now.
      states_nativeLand(census_geometry, cat_places(), input$catname)
    })
    
    cat_places <- shiny::reactive({
      shiny::req(input$catname, input$password)
      category <- stringr::str_remove(input$catname, ", .*$")
      name <- stringr::str_remove(input$catname, "^.*, ")
      get_nativeLand(category, name, input$password, slug)
    })
    places <- shiny::reactive({
      native_places <- native_states()
      if(shiny::isTruthy(input$overlap) & shiny::isTruthy(native_places)) {
        native_places <- dplyr::filter(
          nativeLandUS, .data$Slug %in% native_places$Slug)
      }
      if(shiny::isTruthy(input$catname)) {
        dplyr::bind_rows(cat_places(), native_places)
      } else
        native_places
    })
    
    gg_plot <- shiny::reactive({
      if(shiny::isTruthy(places()) && nrow(places())) {
        out <- ggplot_nativeLand(places(), title = "", label = FALSE)
        # Add `states` overlapping with Native lands.
        if(shiny::isTruthy(input$catstate) &&
           shiny::isTruthy(state_natives()) &&
           nrow(state_natives())) {
          out <- list(out,
                      landMaps:::ggplot_layer_sf(state_natives()))
        }
        out
      } else {
        NULL
      }
    })
    color <- shiny::reactive({
      if(shiny::isTruthy(input$catname) | shiny::isTruthy(input$native_states)) {
        unique(places()$color)
      } else {
        NULL
      }
    })
    ################################
    shiny::reactive({
      list(gg_plot = gg_plot(), color = color())
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
    shiny::h4("Native Land Maps"),
    shiny::fluidRow(
      shiny::column(8, shiny::uiOutput(ns("catname"))),
      shiny::column(4, shiny::checkboxInput(ns("catstate"), "States?", FALSE))
    ),
    shiny::fluidRow(
      shiny::column(8, shiny::uiOutput(ns("native_states"))),
      shiny::column(4, shiny::checkboxInput(ns("overlap"), "Overlap?", FALSE))
    ),
    shiny::uiOutput(ns("password"))
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
  ui <- shiny::fluidPage(
    shiny::titlePanel("Land Shapefiles"),
    shiny::sidebarPanel(
      nativeLandInput("nativeLand"),
      landPlotInput("landPlot")
    ),
    shiny::mainPanel(
      landPlotOutput("landPlot")
    )
  ) 
  server <- function(input, output, session) {
    gg_nativeLand <- nativeLandServer("nativeLand")
    gg_object <- shiny::reactive({
      gg_nativeLand()$gg_plot
    })
    color <- shiny::reactive({
      gg_nativeLand()$color
    })
    landPlotServer("landPlot", gg_object, color)
  }
  shiny::shinyApp(ui, server)
}