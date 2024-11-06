#' Shiny Server for census Example
#'
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname census
#' @importFrom shiny a checkboxInput column fluidPage h4 mainPanel moduleServer
#'             NS plotOutput renderPlot renderUI selectInput selectizeInput
#'             shinyApp sidebarPanel sidebarPanel sliderInput tagList
#'             titlePanel uiOutput updateSelectizeInput
#' @importFrom stringr str_detect
#' @importFrom dplyr arrange filter
censusServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    census_geometry <- readRDS("data/census_geometry.rds")
    census_names <- tidyr::unite(census_geometry, catname, 
                                 category, Name, sep = ": ")$catname
    output$catname <- shiny::renderUI({
      shiny::selectizeInput(ns("catname"), "Name in category:", NULL)
    })
    
    aiannh_states <- shiny::reactive({
      if(!shiny::isTruthy(input$aiannh_states))
        return(NULL)
      census_names[
        # Find AIANNH that end with `, ` followed by `AA`, `AA--XX` or `XX--AA`.
        stringr::str_detect(census_names,
          paste0(", (|[A-Z]+--)", paste(input$aiannh_states, collapse = "|"),
                 "(|--[A-Z]+)$")) &
        census_geometry$category == "aiannh"]
    })
    output$aiannh_states <- shiny::renderUI({
      shiny::selectInput(ns("aiannh_states"), "AIANNH in States:", 
                         state.abb,
                         multiple = TRUE)
    })
    
    state_names <- array(state.name, dimnames = list(state.abb))
    county_states <- shiny::reactive({
      # Census uses whole name
      if(!shiny::isTruthy(input$county_states))
        return(NULL)
      census_names[
        stringr::str_detect(census_names, paste0(", ", 
          paste(state_names[input$county_states], collapse = "|"), "$")) &
          census_geometry$category == "counties"]
    })
    output$county_states <- shiny::renderUI({
      shiny::selectInput(ns("county_states"), "Counties in States:", 
                         state.abb,
                         multiple = TRUE)
    })
    
    catnames <- shiny::reactive({
      shiny::req(input$category)
      census_names[stringr::str_detect(census_names,
        paste0("^", paste(input$category, collapse = "|")))]
    })
    shiny::observeEvent(
      shiny::req(catnames()), {
        shiny::updateSelectizeInput(session, "catname", choices = catnames(),
                                    server = TRUE, selected = input$catname)
      },
      ignoreNULL = FALSE, label = "update_catname")
    
    # places
    shiny::reactive({
      place_names <- unique(c(input$catname,
                              paste("states:", state_names[input$county_states]),
                              paste("states:", state_names[input$aiannh_states]),
                              county_states(), aiannh_states()))
      if(length(place_names)) {
        dplyr::select(
          dplyr::arrange(
            census_geometry[which(census_names %in% place_names),],
              .data$category),
          category, Name, color, description, geometry)
      } else {
        NULL
      }
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
    shiny::h4("Census Maps"),
    shiny::fluidRow(
      shiny::column(4, 
        shiny::selectInput(ns("category"), "Category:", 
                           c("aiannh", "states", "counties"),
                           multiple = TRUE)),
      shiny::column(8, shiny::uiOutput(ns("catname")))
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("aiannh_states"))),
      shiny::column(6, shiny::uiOutput(ns("county_states")))
    )
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
  ui <- shiny::fluidPage(
    shiny::titlePanel("Census Maps"),
    shiny::sidebarPanel(
      censusInput("census"),
      landPlotInput("landPlot")
    ),
    shiny::mainPanel(
      landPlotOutput("landPlot")
    )
  )
  server <- function(input, output, session) {
    census_places <- censusServer("census")
    landPlotServer("landPlot", census_places)
  }
  shiny::shinyApp(ui, server)
}