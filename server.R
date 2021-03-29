library(shiny)
library(shinydashboard)
require(rcrea)
library(lubridate)
library(scales)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras2)
library(dplyr)
library(sf)


# library(creatrajs)
library(plotly)

server <- function(input, output, session) {

    # URL Management
    # observe({
    #     query <- parseQueryString(session$clientData$url_search)
    #     if(!is.null(query$tab)) {
    #         updateNavbarPage(session,
    #                          "nav-page",
    #                          selected = query$tab)
    #     }
    # })

    source(file.path("server", "shared.R"),  local = TRUE)$value
    source(file.path("server", "tab_summary.R"),  local = TRUE)$value
    source(file.path("server", "tab_table.R"),  local = TRUE)$value
    source(file.path("server", "tab_measurements.R"),  local = TRUE)$value
    source(file.path("server", "tab_deweather.R"),  local = TRUE)$value
    source(file.path("server", "tab_trajectories.R"),  local = TRUE)$value

}
