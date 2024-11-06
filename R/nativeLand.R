#' Shiny Server for nativeLand Example
#'
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname nativeLand
#' @importFrom shiny a checkboxInput column fluidPage fluidRow h4 isTruthy
#'             mainPanel moduleServer NS plotOutput renderPlot renderUI
#'             selectInput shinyApp sidebarPanel sliderInput titlePanel uiOutput
#' @importFrom ggplot2 facet_wrap
#' @importFrom dplyr bind_rows distinct filter
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
    nativeLandUS <- readRDS("data/nativeLandUS.rds")
    nativeLand_territories <- dplyr::filter(nativeLandUS,
                                            category == "territories")
    nativeLand_treaties <- dplyr::filter(nativeLandUS,
                                         .data$category == "treaties")
    census_geometry <- readRDS("data/census_geometry.rds")
    native_states <- shiny::reactive({
      if(!shiny::isTruthy(input$native_states))
        return(NULL)
      shiny::req(input$native_states, input$password)
      # Limit to territories for now.
      nativeLand_states(nativeLand_territories, census_geometry, input$native_states)
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
      shiny::req(input$catname, input$password, native_places())
      # Limit to territories for now.
      states_nativeLand(census_geometry, native_places(), input$catname)
    })
    # Find treaties that overlap with selected native lands.
    treaties <- shiny::reactive({
      if(!shiny::isTruthy(input$treaties) | !shiny::isTruthy(input$treaties))
        return(NULL)
      shiny::req(input$treaties, input$password, native_places())
      # Intersect treaties with selected Native places.
      out <- intersect_sf(nativeLand_treaties, native_places())
      if(shiny::isTruthy(input$treaty_overlap)) {
        out <- dplyr::filter(nativeLand_treaties, .data$Name %in% out$Name)
      }
      out
    })
    
    native_places <- shiny::reactive({
      shiny::req(input$catname, input$password)
      category <- stringr::str_remove(input$catname, ", .*$")
      name <- stringr::str_remove(input$catname, "^.*, ")
      get_nativeLand(category, name, input$password, slug)
    })
    
    # places
    shiny::reactive({
      places <- native_states()
      if(shiny::isTruthy(input$overlap) & shiny::isTruthy(places)) {
        places <- dplyr::filter(nativeLandUS, .data$Slug %in% places$Slug)
      }
      if(!is.null(places)) {
        places <- dplyr::select(places, category, Name, color, geometry)
      }
      if(shiny::isTruthy(input$catname)) {
        places <- dplyr::bind_rows(
          dplyr::select(native_places(), category, Name, color, geometry), 
          places)
      }
      # Append states that overlap with selected Native lands. 
      if(shiny::isTruthy(input$catstate) &&
         shiny::isTruthy(state_natives()) &&
         nrow(state_natives())) {
        places <- dplyr::bind_rows(
          dplyr::select(state_natives(), category, Name, color, geometry), 
          places)
      }
      # Append treaties that overlap with selected Native lands. 
      if(shiny::isTruthy(input$treaties) &&
         shiny::isTruthy(treaties()) &&
         nrow(treaties())) {
        places <- dplyr::bind_rows(
          dplyr::select(treaties(), category, Name, color, geometry), 
          places)
      }
      # Remove possible duplication of Native places.
      place_names <- places$Name
      places[!duplicated(place_names),]
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
      shiny::column(6, shiny::checkboxInput(ns("treaties"), "Treaties?", FALSE)),
      shiny::column(6,
        shiny::checkboxInput(ns("treaty_overlap"), "Overlap?", FALSE))),
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
    shiny::titlePanel("Land Maps"),
    shiny::sidebarPanel(
      nativeLandInput("nativeLand"),
      landPlotInput("landPlot")
    ),
    shiny::mainPanel(
      landPlotOutput("landPlot")
    )
  ) 
  server <- function(input, output, session) {
    nativeLand_places <- nativeLandServer("nativeLand")
    landPlotServer("landPlot", nativeLand_places)
  }
  shiny::shinyApp(ui, server)
}