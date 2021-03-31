
data.get_meas <- function(use_cache=T, polls, date_from, date_to, years_rel){

  f <- file.path("results","data","meas.RDS")

  if(file.exists(f) && use_cache){
    readRDS(f)
  }else{
    meas <- do.call("bind_rows",
                    lapply(years_rel, function(year_rel){
                      print(year_rel)
                      rcrea::measurements(
                        date_from=lubridate::date(date_from)+lubridate::years(year_rel),
                        date_to=lubridate::date(date_to)+lubridate::years(year_rel),
                        source="mee",
                        process_id="city_day_mad",
                        deweathered=F,
                        poll=polls,
                        with_metadata=T,
                        with_geometry=F
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

data.keyregions <- function(){
  read.csv("data/station_key2018.csv") %>%
    distinct(location_name=CityEN, keyregion, province=Province)
}

