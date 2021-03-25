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
polls <- c("pm25","pm10")

is_heavy_poll <- function(pm25, pm10, ...){
  (pm25>150) &
    (pm25/pm10 < 0.75)
}


# Find most polluted cities and dates ------------------------------------------

# Get measurements (takes 3min roughly)
tic()
meas <- rcrea::measurements(
  date_from=date_from,
  date_to=date_to,
  source="mee",
  deweathered=F,
  poll=polls,
  with_metadata=T,
  with_geometry=T
)
toc()

saveRDS(meas, file.path("results","data","meas.RDS"))


# Find city for each province that has the highest pollution levels / most heavy pollution days
meas.hp <- meas %>%
  filter(process_id=="city_day_mad") %>%
  tidyr::spread("poll","value") %>%
  mutate(heavy_pollution=is_heavy_poll(pm25=pm25, pm10=pm10))

meas.hp.count <- meas.hp %>%
  group_by(location_id, location_name, gadm1_id, gadm1_name) %>%
  summarise(days_all=n(),
            days_polluted=sum(heavy_pollution)) %>%
  filter(days_polluted>0) %>%
  group_by(gadm1_id) %>%
  arrange(desc(days_polluted))

write.csv(meas.hp.count %>% select(location_name, gadm1_name, days_polluted, days_all),
          file.path("results","data","meas.hp.count.csv"), row.names=F)


# Plots -------------------------------------------------------------------

# Heavy polluted days by province
ggplot(meas.hp %>%
         group_by(gadm1_name, date) %>%
         summarise(heavy_pollution=(sum(heavy_pollution)>=1)) %>%
         # distinct(gadm1_name,date) %>%
         group_by(gadm1_name,date=lubridate::floor_date(date, "week")) %>%
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
location_ids <- unique(meas.hp$location_id)

m.dew <- creadeweather::deweather(
  location_id=location_id,
  poll="pm25",
  add_fire=F,
  calc_fire=T,
  output=c("trend", "anomaly"),
  fire_mode="trajectory",
  training_start_anomaly = lubridate::date(date_from)-lubridate::years(3), # Three years of training
  training_end_anomaly = lubridate::date(date_from)-lubridate::days(1), # We end training just before 2020 winter
  save_weather_filename = file.path("results","data","deweathered",sprintf("weather.%s.RDS",location_id))
  save_trajs_filename = file.path("results","data","deweathered",sprintf("trajs.%s.RDS",location_id))
  )

saveRDS(m.dew, file.path("results","data","deweathered",sprintf("meas.%s.RDS",location_id)))



# Calculate trajectories --------------------------------------------------
# You need a folder with meteorological files
# defined by DIR_HYSPLIT_MET
# Otherwise it will download all files which might take quite some time
duration_hour=72
met_type="gdas1"
height=500

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
