
calc.is_heavy_poll <- function(pm25, pm10, ...){
  (pm25>150) &
    (pm25/pm10 >= 0.75)
}


calc.meas_hp <- function(meas){

  # Find city for each province that has the highest pollution levels / most heavy pollution days
  meas %>%
    filter(process_id=="city_day_mad") %>%
    tidyr::spread("poll","value") %>%
    mutate(heavy_pollution=is_heavy_poll(pm25=pm25, pm10=pm10)) %>%
    filter(heavy_pollution)
}

calc.meas_hp_count <- function(meas.hp){

  meas.hp %>%
    group_by(location_id, location_name, gadm1_id, gadm1_name) %>%
    summarise(days_all=n(),
              days_polluted=sum(heavy_pollution)) %>%
    filter(days_polluted>0) %>%
    group_by(gadm1_id) %>%
    arrange(desc(days_polluted))
}
