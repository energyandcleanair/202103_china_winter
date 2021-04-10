
data.get_meas <- function(use_cache=T, polls, date_from, date_to, years_rel, level="city"){

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
    meas <- do.call("bind_rows",
                    lapply(years_rel, function(year_rel){
                      print(year_rel)
                      rcrea::measurements(
                        location_id=location_id,
                        date_from=lubridate::date(date_from)+lubridate::years(year_rel),
                        date_to=lubridate::date(date_to)+lubridate::years(year_rel),
                        source="mee",
                        process_id=process_id,
                        deweathered=F,
                        poll=polls,
                        with_metadata=T,
                        with_geometry=F,
                        aggregate_level=level
                      ) %>%
                        mutate(season=paste(
                          lubridate::year(date_from)+year_rel,
                          lubridate::year(date_from)+year_rel+1,
                          sep="-")) %>%
                        ungroup()
                    }))
    saveRDS(meas, f)
    return(meas)
  }
}

data.enrich_and_widen <- function(m,
                                  storm_pm10_threshold=300,
                                  storm_pm_ratio=0.75){
  m <- m %>% tidyr::spread("poll", "value")
  m$sand_storm <- (m$pm10 > storm_pm10_threshold) & (m$pm25/m$pm10 < storm_pm_ratio) # THIS AFFECTS A LOT RESULTS
  m$heavy_polluted <- (m$pm25 >= 150) & (m$pm25/m$pm10 >= 0.75)
  m$quarter <- as.character(lubridate::quarter(m$date, with_year=T)) %>% gsub("\\.","Q",.)
  m
}

data.gadm <- function(level=1){

  # We first simplified gadm
  # library(rmapshaper)
  # g <- sf::read_sf(file.path(Sys.getenv("DIR_DATA"),
                             # sprintf("boundaries/gadm/gadm36_%d.shp",level)))
  # g <- g[g$GID_0=="CHN",]
  # rmapshaper::ms_simplify(input = as(g, 'Spatial')) %>%
  # st_as_sf() %>% sf::write_sf("data/boundaries/gadm_simplified.shp")

  return(sf::read_sf("data/boundaries/gadm_simplified.shp"))
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

