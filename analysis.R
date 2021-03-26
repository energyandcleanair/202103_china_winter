# Udpate packages
# require(remotes)
# remotes::install_github("energyandcleanair/rcrea", upgrade=F)
# remotes::install_github("energyandcleanair/creatrajs", upgrade=F)

library(rcrea)
library(remotes)
library(creatrajs)
library(tidyverse)
library(tictoc)

dir.create(file.path("results","data"), recursive=T, showWarnings=F)
dir.create(file.path("results","plots"), recursive=T, showWarnings=F)
dir.create(file.path("cache","trajs"), recursive=T, showWarnings=F)



date_from <- "2020-10-01"
date_to <- "2021-03-31"

years_rel <- seq(0,-4)

polls <- c("pm25","pm10")

# Find most polluted cities and dates ------------------------------------------

# Get measurements (takes 3min roughly)
if(FALSE){
  tic()
  meas <- do.call("bind_rows",
                  lapply(years_rel, function(year_rel){
                    print(year_rel)
                    rcrea::measurements(
                      date_from=lubridate::date(date_from)+lubridate::years(year_rel),
                      date_to=lubridate::date(date_to)+lubridate::years(year_rel),
                      source="mee",
                      deweathered=F,
                      poll=polls,
                      with_metadata=T,
                      with_geometry=T
                    ) %>%
                      mutate(season=paste(
                        lubridate::year(date_from)+year_rel,
                        lubridate::year(date_from)+year_rel+1,
                        sep="-"))
                  }))
  toc()
  saveRDS(meas, file.path("results","data","meas.RDS"))

}else{
  meas <- readRDS(file.path("results","data","meas.RDS"))
}





write.csv(meas.hp.count %>% select(location_name, gadm1_name, days_polluted, days_all),
          file.path("results","data","meas.hp.count.csv"), row.names=F)


# Plots -------------------------------------------------------------------

# Heavy polluted days by province
ggplot(meas.hp %>%
         group_by(gadm1_name, season, date) %>%
         summarise(heavy_pollution=(sum(heavy_pollution)>=1)) %>%
         # distinct(gadm1_name,date) %>%
         group_by(gadm1_name, season, date=lubridate::floor_date(date, "week")) %>%
         summarise(heavy_pollution=sum(heavy_pollution))
) +
  # geom_tile(aes(date,y=0,fill=heavy_pollution)) +
  geom_bar(aes(date, heavy_pollution),
           stat="identity") +
  facet_wrap(~gadm1_name) +
  rcrea::theme_crea() +
  labs(title="Occurence of heavy polluted days in provinces",
       subtitle="Days per week reaching heavy pollution",
       x=NULL, y=NULL) +
  scale_y_continuous(expand=c(0,0), limits=c(0,7))+
  scale_x_datetime(date_minor_breaks ="1 month") +
  theme(panel.grid.minor.x = element_line("grey95"),
        panel.grid.major.x = element_line("grey90"))

ggsave(file.path("results","plots","province.occurence.png"),
       width=8, height=6)




# Deweather --------------------------------------------------
# You need a folder with meteorological files
# defined by DIR_HYSPLIT_MET
# Otherwise it will download all files which might take quite some time
# location_ids <- unique(meas.hp.count$location_id)

duration_hour=72
met_type="gdas1"



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



# Calculate trajectories --------------------------------------------------
# You need a folder with meteorological files
# defined by DIR_HYSPLIT_MET
# Otherwise it will download all files which might take quite some time



meas.hp.trajs <- meas.hp %>%
  filter(heavy_pollution) %>%
  rowwise() %>%
  mutate(meas=list(tibble(pm25=pm25, pm10=pm10)),
         filename=file.path("results","plots",
                            paste(location_id,date,"png",sep=".")))

meas.hp.trajs$trajs <- creatrajs::trajs.get(
  dates=meas.hp.trajs$date,
  location_id=meas.hp.trajs$location_id,
  geometry=meas.hp.trajs$geometry,
  duration_hour=duration_hour,
  met_type=met_type,
  height=height,
  timezone="Asia/Shanghai",
  cache_folder=file.path("cache","trajs"), # Trajectories files are cached here. Won't be recomputed if exist
  parallel=F
)

# Add basemap
meas.hp.trajs <- creatrajs::utils.attach.basemaps(meas.hp.trajs, radius_km = 200, zoom_level = 8)

mapply(
  creatrajs::map.trajs,
  trajs = meas.hp.trajs$trajs,
  basemap = meas.hp.trajs$basemap,
  location_id = meas.hp.trajs$location_id,
  location_name = meas.hp.trajs$location_name,
  date=meas.hp.trajs$date,
  meas=list(NULL),
  duration_hour=duration_hour,
  met_type=met_type,
  height=height,
  filename=meas.hp.trajs$filename,
  SIMPLIFY=F)
