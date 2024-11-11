#' Shiny Server for tmap Plot
#'
#' @param id shiny identifier
#' @param places reactive object with places
#' @param input,output,session shiny server reactives
#' @return reactive server
#' @export
#' @rdname tmap
#' @importFrom shiny checkboxInput column fluidPage fluidRow mainPanel
#'             moduleServer NS plotOutput radioButtons reactive renderPlot
#'             sliderInput sidebarPanel titlePanel uiOutput
#' @importFrom ggspatial annotation_map_tile
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom dplyr bind_rows
#' @importFrom tmap renderTmap tmapOutput tmap_mode tm_basemap tm_borders
#'             tm_shape
tmapServer <- function(id, places = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    valid_places <- shiny::reactive({
      if(shiny::isTruthy(places()) && nrow(places())) {
        sf::st_make_valid(places())
      } else {
        NULL
      }
    })
    color <- shiny::reactive({
      shiny::req(valid_places())
      color <- unique(valid_places()$color)
      names(color) <- color
      color
    })
    # Output Main Plot
    output$tmap_plot <- tmap::renderTmap({
      # https://rdrr.io/cran/tmap/man/renderTmap.html
      tmap::tmap_mode("view")
      tmap::tm_shape(valid_places()) +
        tmap::tm_basemap("OpenStreetMap") +
        # Need to get places colors from valid_places()$color
        tmap::tm_borders(col = "red", lwd = 2)
    })
    output$show_tmap <- shiny::renderUI({
      shiny::req(input$height)
      if(shiny::isTruthy(valid_places()) && nrow(valid_places())) {
        tmap::tmapOutput(ns("tmap_plot"), height = paste0(input$height, "px"))
      } else {
        NULL
      }
    })
  })
}
#' Shiny Module Input for tmap
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname tmap
#' @export
tmapInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::sliderInput(ns("height"), "Height:", 300, 800, 500, 100)
}
#' Shiny Module Output for tmap
#' @param id identifier for shiny reactive
#' @return nothing returned
#' @rdname tmap
#' @export
tmapOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("show_tmap"))
}
#' Shiny Module App for tmap
#' @return nothing returned
#' @rdname tmap
#' @export
tmapApp <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Land Shapefiles"),
    shiny::sidebarPanel(
      censusInput("census"),
      nativeLandInput("nativeLand"),
      tmapInput("tmap")
    ),
    shiny::mainPanel(
      tmapOutput("tmap")
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

    tmapServer("tmap", places)
  }
  shiny::shinyApp(ui, server)
}