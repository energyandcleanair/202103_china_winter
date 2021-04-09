
data.get_meas <- function(use_cache=T, polls, date_from, date_to, years_rel, level="city"){

  f <- file.path("results","data",sprintf("meas.%s.RDS",level))

  if(level=="city"){
    location_id <- NULL
  }else{
    location_id <- read.csv("data/station_key2018.csv") %>%
      filter(keyregion!="none" & !is.na(keyregion)) %>% pull(station_code) %>% tolower()
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

data.enrich_and_widen <- function(m){
  m <- m %>% tidyr::spread("poll", "value")
  m$sand_storm <- (m$pm10 > 300) & (m$pm25/m$pm10 < 0.75) # THIS AFFECTS A LOT RESULTS
  m$heavy_polluted <- (m$pm25 >= 150) & (m$pm25/m$pm10 >= 0.75)
  m$quarter <- as.character(lubridate::quarter(m$date, with_year=T)) %>% gsub("\\.","Q",.)
  m
}

data.gadm <- function(level=1){
  # g <- sf::read_sf(file.path(Sys.getenv("DIR_DATA"),
                             # sprintf("boundaries/gadm/gadm36_%d.shp",level)))
  # g <- g[g$GID_0=="CHN",]
  # g.simplified <- rmapshaper::ms_simplify(input = as(g, 'Spatial')) %>%
    # st_as_sf()
  return(sf::read_sf("data/boundaries/gadm_simplified.shp"))
}

data.keyregions <- function(level="city"){

  if(level=="city"){
    read.csv("data/station_key2018.csv") %>%
      distinct(location_name=CityEN, keyregion, province=Province)
  }else{
    read.csv("data/station_key2018.csv") %>%
      distinct(location_id=tolower(station_code),
               location_name=station_name,
               city_name=CityEN, keyregion, province=Province)
  }

}

