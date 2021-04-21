
data.get_meas <- function(use_cache=T, polls, date_from, date_to, level="city"){

  f <- file.path("results","data",sprintf("meas.%s.RDS",level))

  if(level=="city"){
    location_id <- NULL
    process_id <- "city_day_mad"
  }else{
    location_id <- read.csv("data/station_key2018.csv") %>%
      filter(keyregion2018!="none" & !is.na(keyregion2018)) %>% pull(station_code) %>% tolower()
    process_id <- "station_day_mad"
  }

  if(file.exists(f) && use_cache){
    readRDS(f)
  }else{
    meas <- rcrea::measurements(
                        location_id=location_id,
                        date_from=date_from,
                        date_to=date_to,
                        source="mee",
                        process_id=process_id,
                        deweathered=F,
                        poll=polls,
                        with_metadata=T,
                        with_geometry=F,
                        aggregate_level=level
                      ) %>% ungroup()

    saveRDS(meas, f)
    return(meas)
  }
}

data.enrich_and_widen <- function(m,
                                  storm_pm10_threshold,
                                  storm_pm_ratio){
  m <- m %>% tidyr::spread("poll", "value")
  m$sand_storm <- (m$pm10 > storm_pm10_threshold) & (m$pm25/m$pm10 < storm_pm_ratio) # THIS AFFECTS A LOT RESULTS
  m$heavy_polluted <- (m$pm25 >= 150) | (m$pm10 >= 350) | (m$no2 > 280)
  m$quarter <- as.character(lubridate::quarter(m$date, with_year=T)) %>% gsub("\\.","Q",.)
  m
}

data.weather <- function(){
  fs <- list.files("results/data/deweathered/","weather*", full.names=T)
  lapply(fs, readRDS) %>%
    do.call(bind_rows, .) %>%
    rename(location_id=station_id) %>%
    dplyr::select(-c(timezone)) %>%
    tidyr::unnest(meas_weather)
}

data.gadm <- function(level=1){

  # We first simplified gadm
  # library(rmapshaper)
  # g <- sf::read_sf(file.path(Sys.getenv("DIR_DATA"),
                             # sprintf("boundaries/gadm/gadm36_%d.shp",level)))
  # g <- g[g$GID_0=="CHN",]
  # rmapshaper::ms_simplify(input = as(g, 'Spatial')) %>%
  # st_as_sf() %>% sf::write_sf("data/boundaries/gadm_simplified.shp")

  # require(GADMTools)
  # g.chn <- GADMTools::gadm_sf.loadCountries("CHN", level=1, simplify=0.01)
  # g.twn <- GADMTools::gadm_sf.loadCountries("TWN", level=0, simplify=0.01)
  # g <- bind_rows(g.chn$sf, g.twn$sf)
  # sf::write_sf(g, "data/boundaries/gadm_simplified2.shp")

  return(sf::read_sf("data/boundaries/gadm_simplified2.shp"))
}

data.gadm.wneighbours <- function(level=1){

  # We first simplified gadm
  library(rmapshaper)

  # Level 1 for China, 0 for other countries
  # g.chn <- sf::read_sf(
  #   file.path(Sys.getenv("DIR_DATA"),
  #             sprintf("boundaries/gadm/gadm36_%d.shp",1))) %>%
  #   filter(GID_0=="CHN")
  #
  # margin <- 10
  # bbox <- sf::st_bbox(g.chn) + c(-1,-1,1,1)*margin
  #
  #
  # g.other <- sf::read_sf(
  #   file.path(Sys.getenv("DIR_DATA"),
  #             sprintf("boundaries/gadm/gadm36_%d.shp",0))) %>%
  #   filter(GID_0!="CHN")
  #
  # g.other <- g.other %>% sf::st_crop(bbox)
  #
  # g = rbind(g.chn, g.other)
  # rmapshaper::ms_simplify(input = as(g, 'Spatial')) %>%
  # st_as_sf() %>% sf::write_sf("data/boundaries/gadm_wneighbours_simplified.shp")

  # require(GADMTools)

  # g.neighbours <- GADMTools::gadm_sf.loadCountries(c("CHN","TWN","RUS","KAZ","MNG"), level=1, simplify=0.01)
  # g.twn <- GADMTools::gadm_sf.loadCountries("TWN", level=0, simplify=0.01)
  # g <- bind_rows(g.chn$sf, g.twn$sf)
  # sf::write_sf(g, "data/boundaries/gadm_simplified2.shp")

  return(sf::read_sf("data/boundaries/gadm_wneighbours_simplified.shp"))
}

data.capitals <- function(m.c){
 read_csv(file.path("data","cities.csv")) %>%
    filter(capital %in% c("primary","admin")) %>%
    dplyr::select(location_name=city,
           gadm1_name=admin_name) %>%
    mutate(location_name=recode(location_name,
                                "Ürümqi"="Urumqi",
                                "Xi’an"="Xi'an")) %>%
    left_join(m.c %>%
                distinct(location_name, location_id, gadm1_name) %>%
                mutate(gadm1_name=recode(gadm1_name,
                                         "Xinjiang Uygur"="Xinjiang",
                                         "Nei Mongol"="Inner Mongolia",
                                         "Ningxia Hui"="Ningxia",
                                         "Xizang"="Tibet")))
}

data.keyregions <- function(level="city"){

  if(level=="city"){
    read.csv("data/station_key2018.csv") %>%
      distinct(location_name=CityEN, keyregion2018, province=Province)
  }else{
    read.csv("data/station_key2018.csv") %>%
      distinct(location_id=tolower(station_code),
               location_name=station_name,
               city_name=CityEN, keyregion2018, province=Province)
  }

}

