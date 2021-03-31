require(creadeweather)

date_from <- "2020-10-01"
date_to <- "2021-03-31"
training_end <- lubridate::date(date_from)-lubridate::years(1)-lubridate::days(1)
training_start <- training_end - lubridate::years(3)

m.dew <- creadeweather::deweather(
  source="mee",
  poll="pm25",
  output=c("trend","anomaly"),
  training_start_anomaly = training_start,
  training_end_anomaly = training_end,
  upload_results = F
)

saveRDS(m.dew)



# Old ---------------------------------------------------------------------


# Deweather --------------------------------------------------
# You need a folder with meteorological files
# defined by DIR_HYSPLIT_MET
# Otherwise it will download all files which might take quite some time
# location_ids <- unique(meas.hp.count$location_id)



# Deweather capitals first
capitals <- read_csv(file.path("data","cities.csv")) %>%
  filter(capital %in% c("primary","admin")) %>%
  select(location_name=city,
         gadm1_name=admin_name) %>%
  mutate(location_name=recode(location_name,
                              "Ürümqi"="Urumqi",
                              "Xi’an"="Xi'an")) %>%
  left_join(meas %>%
              distinct(location_name, location_id, gadm1_name) %>%
              mutate(gadm1_name=recode(gadm1_name,
                                       "Xinjiang Uygur"="Xinjiang",
                                       "Nei Mongol"="Inner Mongolia",
                                       "Ningxia Hui"="Ningxia",
                                       "Xizang"="Tibet")))
# We found everyone but Haikou-Hainan
dir.create(file.path("results","data","deweathered"), showWarnings = F, recursive = T)
location_ids <- setdiff(unique(capitals$location_id), NA)

for(location_id in location_ids){
  print(location_id)

  weather_filename <- file.path("results","data","deweathered",sprintf("weather.%s.RDS",location_id))
  trajs_filename <- file.path("results","data","deweathered",sprintf("trajs.%s.RDS",location_id))
  meas_filename <- file.path("results","data","deweathered",sprintf("meas.%s.RDS",location_id))

  if(!file.exists(meas_filename)){
    print("Deweathering")
    m.dew <- creadeweather::deweather(
      location_id=location_id,
      poll="pm25",
      source="mee",
      upload_results = F,
      add_fire=F,
      output=c("trend", "anomaly"),
      training_start_anomaly = lubridate::date(date_from)-lubridate::years(3), # Three years of training
      training_end_anomaly = lubridate::date(date_from)-lubridate::days(1), # We end training just before 2020 winter
      save_weather_filename = weather_filename
    )
    saveRDS(m.dew, meas_filename)
  }

  # Calculate trajectories at pbl height
  weather <- readRDS(weather_filename) %>%
    select(location_id=station_id, geometry, meas_weather) %>%
    tidyr::unnest(cols=meas_weather) %>%
    filter(date>=date_from,
           date<=date_to) %>%
    filter(!is.na(pbl_min))

  if(!file.exists(trajs_filename)){
    print("Trajectories")
    trajs <- creatrajs::trajs.get(
      dates=weather$date,
      location_id=weather$location_id,
      geometry=weather$geometry,
      duration_hour=duration_hour,
      met_type=met_type,
      height=(weather$pbl_min+weather$pbl_max)/2,
      timezone="Asia/Shanghai",
      cache_folder=file.path("cache","trajs"), # Trajectories files are cached here. Won't be recomputed if exist
      parallel=F
    )

    saveRDS(trajs, trajs_filename)
  }
}
