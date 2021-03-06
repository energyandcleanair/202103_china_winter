library(shiny)
library(shinydashboard)

library(shinyBS)
library(leaflet)
library(plotly)

ui <- navbarPage(
    title=div(img(src="crea_logo.svg",
                  height=44)),
    windowTitle="CREA - Air Quality Dashboard",
    theme = "theme.css",
    id = "nav-page",

    source(file.path("ui", "tab_summary.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_table.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_trajectories.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_measurements.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_deweather.R"),  local = TRUE)$value

)


