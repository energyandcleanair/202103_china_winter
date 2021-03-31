# Udpate packages
# require(remotes)
# remotes::install_github("energyandcleanair/rcrea", upgrade=F)
# remotes::install_github("energyandcleanair/creatrajs", upgrade=F)

library(rcrea)
library(remotes)
library(creatrajs)
library(tidyverse)
library(tictoc)
library(rmapshaper)
library(sf)

dir.create(file.path("results","data"), recursive=T, showWarnings=F)
dir.create(file.path("results","plots"), recursive=T, showWarnings=F)
dir.create(file.path("results","maps"), recursive=T, showWarnings=F)
dir.create(file.path("cache","trajs"), recursive=T, showWarnings=F)

source('data.R')
source('plots.R')
source('maps.R')
source('utils.R')


date_from <- "2020-10-01"
date_to <- "2021-03-31"
years_rel <- seq(0,-4)
polls <- c("pm25","pm10")

meas <- data.get_meas(use_cache=T, polls=polls, date_from=date_from, date_to=date_to, years_rel=years_rel)
meas <- meas %>% tidyr::spread("poll", "value")
meas$sand_storm <- (meas$pm10 > 300) & (meas$pm25/meas$pm10 < 0.75) # THIS AFFECTS A LOT RESULTS
meas$heavy_polluted <- (meas$pm25 >= 150) & (meas$pm25/meas$pm10 >= 0.75)
meas$quarter <- as.character(lubridate::quarter(meas$date, with_year=T)) %>% gsub("\\.","Q",.)


#1 % change in PM2.5 concentrations in 2020Q4 and 2021Q1, compared with 2019Q4 and 2019Q1, respectively, with sand storm days eliminated and both observed and deweathered data, by city, province and key region

# Province
m.change.province <- meas %>%
  filter(quarter %in% c("2020Q4","2021Q1","2019Q4","2020Q1"),
         !sand_storm) %>%
  group_by(gadm1_id, province=gadm1_name, quarter) %>%
  summarise_at(c("pm25"),mean, na.rm=T) %>%
  tidyr::spread("quarter","pm25") %>%
  mutate(
    change_Q1_rel=`2021Q1`/`2020Q1`-1,
    change_Q4_rel=`2020Q4`/`2019Q4`-1)

write.csv(m.change.province, "results/data/change_province.csv", row.names = F)
map.change_province(m.change.province)


# City
m.change.city <- meas %>%
  filter(quarter %in% c("2020Q4","2021Q1","2019Q4","2020Q1"),
         !sand_storm) %>%
  group_by(location_id, province=gadm1_name, city=location_name, quarter) %>%
  summarise_at(c("pm25"), mean, na.rm=T) %>%
  tidyr::spread("quarter","pm25") %>%
  mutate(
    change_Q1_rel=`2021Q1`/`2020Q1`-1,
    change_Q4_rel=`2020Q4`/`2019Q4`-1)

write.csv(m.change.city, "results/data/change_city.csv", row.names = F)
map.change_city(m.change.city)

# Key regions
m.change.keyregion <- meas %>%
  rename(province=gadm1_name) %>%
  left_join(data.keyregions()) %>%
  filter(quarter %in% c("2020Q4","2021Q1","2019Q4","2020Q1"),
         !sand_storm) %>%
  group_by(keyregion, quarter) %>%
  summarise_at(c("pm25"), mean, na.rm=T) %>%
  tidyr::spread("quarter","pm25") %>%
  mutate(
    change_Q1_rel=`2021Q1`/`2020Q1`-1,
    change_Q4_rel=`2020Q4`/`2019Q4`-1) %>%
  dplyr::filter(!is.na(keyregion))

write.csv(m.change.keyregion, "results/data/change_keyregion.csv", row.names = F)


# Map change
map_change <- map.change_interpolated(meas, res=0.1)
png("results/maps/map_change_interpolated.png", width = 800, height=600)
print(map_change)
dev.off()


# -number of heavy polluted days, excluding sand storms, in the past 12 months up to end of March, by city and province (worst city)
m.hp.city <- meas %>%
  filter(heavy_polluted,
         !sand_storm,
         date>="2020-04-01") %>%
  group_by(city=location_name, province=gadm1_name) %>%
  summarise(count=n())
write.csv(m.hp.city, "results/data/heavy_polluted_per_city.csv")


m.hp.worsecity <- m.hp.city %>%
  group_by(province) %>%
  filter(count==max(count)) %>%
  rename(worse_city=city)
write.csv(m.hp.city, "results/data/heavy_polluted_per_province.csv")


# back trajectories for the heavy polluted days, for the worst city in each province and for provincial capitals

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

worse_cities <- meas %>%
  filter(heavy_polluted,
         date>="2020-04-01") %>%
  group_by(location_id, location_name, gadm1_name) %>%
  summarise(count=n()) %>%
  group_by(gadm1_name) %>%
  filter(count==max(count))

location_ids <- unique(c(capitals$location_id, worse_cities$location_id))

duration_hour=72
met_type="gdas1"


meas.hp.trajs <- meas %>%
  left_join(rcrea::cities(id=unique(meas$location_id), with_geometry=T) %>% dplyr::select(location_id=id, geometry)) %>%
  filter(location_id %in% location_ids) %>%
  filter(heavy_polluted,
         !sand_storm) %>%
  distinct(location_id, location_name, gadm1_name, geometry, date) %>%
  rowwise() %>%
  mutate(
    # meas=list(tibble(pm25=pm25, pm10=pm10)),
    filename=file.path("results","plots",
                       sprintf("%s_%s_%s.png",
                               tolower(gadm1_name),
                               tolower(location_name),
                               gsub("-","",as.character(date)))))



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


# Old ---------------------------------------------------------------------




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
