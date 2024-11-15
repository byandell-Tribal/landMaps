nativeLandSlug <- readRDS("data/NativeLandSlug.rds")
nativeLandUS <- readRDS("data/nativeLandUS.rds")
census_geometry <- readRDS("data/census_geometry.rds")

ui <- shiny::fluidPage(
  shiny::titlePanel("Land Maps"),
  shiny::sidebarPanel(
    landMapsInput("landMaps")
  ),
  shiny::mainPanel(
    landMapsOutput("landMaps"),
    landMapsUI("landMaps")
  )
) 
server <- function(input, output, session) {
  landMapsServer("landMaps",
                 nativeLandSlug, nativeLandUS, census_geometry)
}
shiny::shinyApp(ui, server)
