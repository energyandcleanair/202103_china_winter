require(rcrea)
require(DT)
require(shinyWidgets)
library(shinycssloaders)
library(countrycode)


# deweathered data folder ------------------------------------------------------------
deweathered.folder <- "results/data/deweathered"
trajs.folder <- "results/data/trajectories"

trajs_gibs_layers <- list(
  "Aerosol Optical Depth"=c("MODIS_Combined_Value_Added_AOD"),
  "True Color"=c(
    # "MODIS_Terra_CorrectedReflectance_TrueColor",
    # "MODIS_Aqua_CorrectedReflectance_TrueColor",
    "VIIRS_SNPP_CorrectedReflectance_TrueColor")
)

sentinel_url = "https://creodias.sentinel-hub.com/ogc/wms/a6993002-3fe5-45f9-8318-e23b1a5f21b1"
sentinel_layers = list("NO2"="NO2VISUALISED")
