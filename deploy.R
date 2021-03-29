
deployShinyApp <- function() {

  if(!require(devtools)) install.packages('devtools')
  devtools::install_github("energyandcleanair/rcrea", force=T, upgrade="never")
  devtools::install_github("energyandcleanair/creatrajs", force=T, upgrade="never")
  devtools::install_github("energyandcleanair/leaflet.extras2", force=T, upgrade="never")

  library(leaflet.extras2)
  try(dotenv::load_dot_env(), silent=T)
  try(readRenviron(".Renviron"), silent=T)

  rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPP_ACCOUNT"),
                            token=Sys.getenv("SHINYAPP_TOKEN"),
                            secret=Sys.getenv("SHINYAPP_SECRET"))

  # We could deploy like this:
  # rsconnect::deployApp(appDir)
  # but it would miss the auth file that is not pushed to Github

  rsconnect::deployApp(".",
                       appName="chinawinter",
                       appFiles = c(
                         ".Renviron",
                         "global.R",
                         "ui.R",
                         "server.R",
                         "data",
                         "results",
                         "ui",
                         "server",
                         "www"),
                       account = Sys.getenv("SHINYAPP_ACCOUNT"),
                       forceUpdate = TRUE)

}
