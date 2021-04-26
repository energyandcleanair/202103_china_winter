# Udpate packages
# require(remotes)
# remotes::install_github("energyandcleanair/rcrea", upgrade=F)
# remotes::install_github("energyandcleanair/creatrajs", upgrade=F)

library(magrittr)
library(rcrea)
library(remotes)
library(creatrajs)
library(tidyverse)
library(tictoc)
library(sf)
library(pbapply)
library(sp)
library(rasterVis)
library(gstat)
library(RColorBrewer)
library(tmaptools)

dir.create(file.path("results","data"), recursive=T, showWarnings=F)
dir.create(file.path("results","plots"), recursive=T, showWarnings=F)
dir.create(file.path("results","maps"), recursive=T, showWarnings=F)
dir.create(file.path("results","maps","trajs"), recursive=T, showWarnings=F)
dir.create(file.path("cache","trajs"), recursive=T, showWarnings=F)

source('data.R')
source('plots.R')
source('maps.R')
source('utils.R')



date_from <- "2018-10-01"
date_to <- "2021-03-31"
polls <- c("pm25","pm10","no2")

# Sandstorm criteria: found in desandstorm.R when comparing with official numbers
storm_pm10_threshold <- 400
storm_pm_ratio <- 0.8


# m.c = city-level measurements
# m.s = station-level measurements
m.c <- data.get_meas(use_cache=T, polls=polls, level="city", date_from=date_from, date_to=date_to) %>%
  data.enrich_and_widen(storm_pm10_threshold=storm_pm10_threshold,
                        storm_pm_ratio=storm_pm_ratio)
m.s <- data.get_meas(use_cache=T, polls=polls, level="station", date_from=date_from, date_to=date_to) %>%
  data.enrich_and_widen(storm_pm10_threshold=storm_pm10_threshold,
                       storm_pm_ratio=storm_pm_ratio)

#1 % change in PM2.5 concentrations in 2020Q4 and 2021Q1, compared with 2019Q4 and 2019Q1, respectively, with sand storm days eliminated and both observed and deweathered data, by city, province and key region
quarters <- c("2018Q4","2019Q1","2020Q4","2021Q1","2019Q4","2020Q1")

# Province
m.change.province <- m.c %>%
  filter(quarter %in% quarters,
         !sand_storm) %>%
  group_by(gadm1_id, province=gadm1_name, province_zh=gadm1_name_zh, quarter) %>%
  summarise_at(c("pm25"),mean, na.rm=T) %>%
  tidyr::spread("quarter","pm25") %>%
  mutate(
    `2021Q1_vs_2019Q1_str`=paste0(round(`2021Q1`/`2019Q1`-1,3)*100,"%"), # For csv
    `2020Q4_vs_2019Q4_str`=paste0(round(`2020Q4`/`2019Q4`-1,3)*100,"%"), # For csv
    `2021Q1_vs_2019Q1`=`2021Q1`/`2019Q1`-1, # For map
    `2020Q4_vs_2019Q4`=`2020Q4`/`2019Q4`-1)

write.csv(m.change.province, "results/data/change_province.csv", row.names = F)
map.change_province(m.change.province, zh_en="zh")


# City
m.change.city <- m.c %>%
  rename(province=gadm1_name) %>%
  left_join(data.keyregions()) %>%
  filter(quarter %in% quarters,
         !sand_storm) %>%
  group_by(location_id, keyregion2018, province, city=location_name, quarter) %>%
  summarise_at(c("pm25"), mean, na.rm=T) %>%
  tidyr::spread("quarter","pm25") %>%
  mutate(
    `2021Q1_vs_2019Q1_str`=paste0(round(`2021Q1`/`2019Q1`-1,3)*100,"%"), # For csv
    `2020Q4_vs_2019Q4_str`=paste0(round(`2020Q4`/`2019Q4`-1,3)*100,"%"), # For csv
    `2021Q1_vs_2019Q1`=`2021Q1`/`2019Q1`-1, # For map
    `2020Q4_vs_2019Q4`=`2020Q4`/`2019Q4`-1 # For map
  )

write.csv(m.change.city, "results/data/change_city.csv", row.names = F)
map.change_city(m.change.city)
map.change_city_keyregion(m.change.city)
map.change_city_pols(m.change.city)

# Key regions
m.change.keyregion <- m.c %>%
  rename(province=gadm1_name) %>%
  left_join(data.keyregions()) %>%
  filter(quarter %in% quarters,
         !sand_storm) %>%
  group_by(keyregion2018, quarter) %>%
  summarise_at(c("pm25"), mean, na.rm=T) %>%
  tidyr::spread("quarter","pm25") %>%
  mutate(
    `2021Q1_vs_2019Q1_str`=paste0(round(`2021Q1`/`2019Q1`-1,3)*100,"%"), # For csv
    `2020Q4_vs_2019Q4_str`=paste0(round(`2020Q4`/`2019Q4`-1,3)*100,"%"), # For csv
    `2021Q1_vs_2019Q1`=`2021Q1`/`2019Q1`-1, # For map
    `2020Q4_vs_2019Q4`=`2020Q4`/`2019Q4`-1 # For map
  ) %>%
  dplyr::filter(!is.na(keyregion2018))

write.csv(m.change.keyregion, "results/data/change_keyregion.csv", row.names = F)



#TODO average by station & monthly + compare with monthly official numbers
# 2x2 different ways
# City or station level
# With or without sand storm
# City w/o sandstorm
avg1 <- m.c %>%
  rename(province=gadm1_name) %>%
  left_join(data.keyregions(), by=c("location_name","province")) %>%
  filter(quarter %in% quarters,
         !sand_storm) %>%
  group_by(keyregion2018, quarter) %>%
  summarise_at(c("pm25"), mean, na.rm=T) %>%
  tidyr::spread("quarter","pm25") %>%
  dplyr::filter(!is.na(keyregion2018) & keyregion2018!="none") %>%
  mutate(method="City without sandstorm")

hp1 <- m.c %>%
  rename(province=gadm1_name) %>%
  full_join(data.keyregions(), by=c("location_name","province")) %>%
  filter(quarter %in% quarters,
         !sand_storm,
         heavy_polluted) %>%
  group_by(keyregion2018, location_name, quarter) %>%
  summarise(count=n()) %>%
  group_by(keyregion2018, quarter) %>%
  summarise(count=mean(count)) %>%
  dplyr::filter(!is.na(keyregion2018) & keyregion2018!="none") %>%
  mutate(method="City without sandstorm") %>%
  tidyr::spread("quarter","count")

# City w sandstorm
avg2 <- m.c %>%
  rename(province=gadm1_name) %>%
  left_join(data.keyregions(), by=c("location_name","province")) %>%
  filter(quarter %in% quarters) %>%
  group_by(keyregion2018, quarter) %>%
  summarise_at(c("pm25"), mean, na.rm=T) %>%
  tidyr::spread("quarter","pm25") %>%
  dplyr::filter(!is.na(keyregion2018) & keyregion2018!="none") %>%
  mutate(method="City with sandstorm")

hp2 <- m.c %>%
  rename(province=gadm1_name) %>%
  full_join(data.keyregions(), by=c("location_name","province")) %>%
  filter(quarter %in% quarters,
         heavy_polluted) %>%
  group_by(keyregion2018, location_name, quarter) %>%
  summarise(count=n()) %>%
  group_by(keyregion2018, quarter) %>%
  summarise(count=mean(count)) %>%
  dplyr::filter(!is.na(keyregion2018) & keyregion2018!="none") %>%
  mutate(method="City with sandstorm") %>%
  tidyr::spread("quarter","count")

# Station w/o sandstorm
avg3 <- m.s %>%
  left_join(data.keyregions("station"), by=c("location_id")) %>%
  filter(quarter %in% quarters,
         !sand_storm) %>%
  group_by(keyregion2018, quarter) %>%
  summarise_at(c("pm25"), mean, na.rm=T) %>%
  tidyr::spread("quarter","pm25") %>%
  dplyr::filter(!is.na(keyregion2018)) %>%
  mutate(method="Station without sandstorm")

hp3 <- m.s %>%
  rename(province=gadm1_name) %>%
  full_join(data.keyregions("station"), by=c("location_id")) %>%
  filter(quarter %in% quarters,
         heavy_polluted,
         !sand_storm) %>%
  group_by(keyregion2018, city_name, quarter) %>%
  summarise(count=n()) %>%
  group_by(keyregion2018, quarter) %>%
  summarise(count=mean(count)) %>%
  dplyr::filter(!is.na(keyregion2018) & keyregion2018!="none") %>%
  mutate(method="Station without sandstorm") %>%
  tidyr::spread("quarter","count")

# Station with sandstorm
avg4 <- m.s %>%
  left_join(data.keyregions("station"), by=c("location_id")) %>%
  filter(quarter %in% quarters) %>%
  group_by(keyregion2018, quarter) %>%
  summarise_at(c("pm25"), mean, na.rm=T) %>%
  tidyr::spread("quarter","pm25") %>%
  dplyr::filter(!is.na(keyregion2018)) %>%
  mutate(method="Station with sandstorm")

hp4 <- m.s %>%
  rename(province=gadm1_name) %>%
  full_join(data.keyregions("station"), by=c("location_id")) %>%
  filter(quarter %in% quarters,
         heavy_polluted) %>%
  group_by(keyregion2018, city_name, quarter) %>%
  summarise(count=n()) %>%
  group_by(keyregion2018, quarter) %>%
  summarise(count=mean(count)) %>%
  dplyr::filter(!is.na(keyregion2018) & keyregion2018!="none") %>%
  mutate(method="Station with sandstorm") %>%
  tidyr::spread("quarter","count")

avg.official <- tibble(
  keyregion2018=c(
    "2+26","2+26","2+26",
    "Fenwei","Fenwei","Fenwei",
    "YRD","YRD","YRD"),
  quarter=rep(c("2018Q4","2019Q4","2019Q1"),3),
  value=c(
    71,63,90,
    62,64,94,
    48,47,61),
  method="Official"
) %>% tidyr::spread("quarter","value")

hp.official <- tibble(
  keyregion2018=c(
    "2+26","2+26","2+26",
    "Fenwei","Fenwei","Fenwei",
    "YRD","YRD","YRD"),
  quarter=rep(c("2018Q4","2019Q4","2019Q1"),3),
  value=c(
    6,5,14,
    3,5,16,
    1,0,2),
  method="Official"
) %>% tidyr::spread("quarter","value")

bind_rows(avg.official, avg2, avg1, avg4, avg3) %>%
  arrange(keyregion2018) %>%
  mutate_at(quarters, round, digits=1) %>%
  write.csv("results/data/keyregion_methodologies.csv", row.names = F, na = "-")

bind_rows(hp.official, hp2, hp1, hp4, hp3) %>%
  arrange(keyregion2018) %>%
  mutate_if(is.numeric, round, digits=1) %>%
  write.csv("results/data/keyregion_methodologies_hp.csv", row.names = F, na = "-")


m.hp.keyregion <-  m.c %>%
  rename(province=gadm1_name) %>%
  full_join(data.keyregions(), by=c("location_name","province")) %>%
  filter(quarter %in% quarters,
         !sand_storm,
         heavy_polluted) %>%
  group_by(keyregion2018, location_name, quarter) %>%
  summarise(count=n()) %>%
  group_by(keyregion2018, quarter) %>%
  summarise(count=mean(count)) %>%
  dplyr::filter(!is.na(keyregion2018) & keyregion2018!="none")

plots.hp_keyregion(m.hp.keyregion %>% filter(keyregion2018!="PRD"), width=10, nrow=1)
write.csv(m.hp.keyregion, "results/data/heavypolluted_keyregion.csv", row.names = F, na = "-")


m.hp.province <-  m.c %>%
  rename(province=gadm1_name) %>%
  full_join(data.keyregions(), by=c("location_name","province")) %>%
  filter(lubridate::year(date)==2020,
         quarter %in% quarters,
         !sand_storm,
         heavy_polluted) %>%
  group_by(province, location_id, month=lubridate::floor_date(date,"months")) %>%
  summarise(count=n()) %>%
  group_by(province, month) %>%
  summarise(count=mean(count))

plots.hp_province(m.hp.province, width=10, nrow=1)

# Map change
map_change <- map.change_interpolated(m.c, res=0.1)
png("results/maps/map_change_interpolated.png", width = 1200, height=600)
print(map_change)
dev.off()


# number of heavy polluted days, excluding sand storms in 2020
m.hp.city <- m.c %>%
  filter(!sand_storm,
         # date>="2020-04-01",
         # date<"2021-04-01"
         lubridate::year(date)==2020
         ) %>%
  group_by(location_id, city=location_name, province=gadm1_name) %>%
  summarise(count=sum(heavy_polluted))
map.hp_city_pols(m.hp.city)


# -number of heavy polluted days, excluding sand storms, in the past 12 months up to end of March, by city and province (worst city)
m.hp.city <- m.c %>%
  filter(heavy_polluted,
         !sand_storm,
         date>="2020-04-01") %>%
  group_by(city=location_name, province=gadm1_name) %>%
  summarise(count=n())
write.csv(m.hp.city, "results/data/heavy_polluted_per_city.csv", row.names = F)


m.hp.worsecity <- m.hp.city %>%
  group_by(province) %>%
  filter(count==max(count)) %>%
  rename(worse_city=city)
write.csv(m.hp.worsecity, "results/data/heavy_polluted_per_province.csv", row.names = F)


# Can you generate the heavy polluted days by cities in 2020Q4 and 2021Q1
m.count.city <- m.c %>%
  filter(quarter %in% quarters,
         !sand_storm,
         heavy_polluted) %>%
  group_by(location_id, province=gadm1_name, city=location_name, quarter) %>%
  summarise(ndays=n()) %>%
  tidyr::spread("quarter","ndays") %>%
  replace(is.na(.), 0)
write.csv(m.count.city, "results/data/heavy_polluted_per_city_quarter.csv", row.names = F)


# Sand storms -------------------------------------------------------------

m.storm.keyregion <- m.c %>%
  rename(province=gadm1_name) %>%
  left_join(data.keyregions()) %>%
  filter(quarter %in% quarters,
         sand_storm) %>%
  mutate(season=recode(quarter,
                       "2018Q4"="2018-2019",
                       "2019Q1"="2018-2019",
                       "2019Q4"="2019-2020",
                       "2020Q1"="2019-2020",
                       "2020Q4"="2020-2021",
                       "2021Q1"="2020-2021",)) %>%
  distinct(keyregion2018, season, date) %>%
  group_by(keyregion2018, season) %>%
  summarise(count=n()) %>%
  dplyr::filter(!is.na(keyregion2018))

plots.sandstorm_keyregion(m.storm.keyregion)
write.csv(m.storm.keyregion, "results/data/sandstorm_keyregion.csv", row.names = F)

# weather and meas combined
weather <- data.weather() %>%
  dplyr::select(-c(geometry, poll, unit, source, process_id, country, timezone, value, visibility))

meas <- m.c %>%
  dplyr::select(location_id, location_name, heavy_polluted, sand_storm, pm25, pm10, date) %>%
  left_join(data.keyregions()) %>%
  left_join(weather)

saveRDS(meas, file.path("results","data","meas.weather.RDS"))


<<<<<<< HEAD

#  Windrose ---------------------------------------------------------------

map.windrose(meas=m.c, weather, filename="test.png", met_type="", duration_hour="", height="")

=======
# Trajectories ------------------------------------------------------------
>>>>>>> 695b3515d2102f0a42b0172e528ece913efbbd8d

# back trajectories for the heavy polluted days, for the worst city in each province and for provincial capitals
#
capitals <- read_csv(file.path("data","cities.csv")) %>%
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

worse_cities <- m.c %>%
  filter(heavy_polluted,
         !sand_storm,
         date>="2020-04-01") %>%
  group_by(location_id, location_name, gadm1_name) %>%
  summarise(count=n()) %>%
  group_by(gadm1_name) %>%
  filter(count==max(count))

location_ids <- unique(c(capitals$location_id, worse_cities$location_id))

duration_hour=72
met_type="gdas1"
height=50

city.locations <- sf::st_as_sf(rcrea::cities(id=unique(m.c$location_id), with_geometry=T) %>% dplyr::select(location_id=id, geometry))
sf::st_geometry(city.locations)[city.locations$location_id=="beijing_chn.2_1_cn"] = st_point(c(116.4074,39.9042))

meas.hp.trajs <- m.c %>%
  left_join(city.locations) %>%
  filter(location_id %in% location_ids) %>%
  filter(heavy_polluted,
         !sand_storm,
         date>="2020-10-01",
         date<="2021-03-31") %>%
  distinct(location_id, location_name, gadm1_name, geometry, date) %>%
  rowwise() %>%
  mutate(
    # m.c=list(tibble(pm25=pm25, pm10=pm10)),
    filename=file.path("results","plots",
                       sprintf("%s_%s_%s.png",
                               tolower(gadm1_name),
                               tolower(location_name),
                               gsub("-","",as.character(date))))
   )



meas.hp.trajs$trajs <- creatrajs::trajs.get(
  dates=meas.hp.trajs$date,
  location_id=meas.hp.trajs$location_id,
  geometry=meas.hp.trajs$geometry,
  duration_hour=duration_hour,
  hours=seq(4,20, 4),
  met_type=met_type,
  height=height,
  timezone="Asia/Shanghai",
  cache_folder=file.path("cache","trajs"), # Trajectories files are cached here. Won't be recomputed if exist
  parallel=F
)


###########################
# Plot by location x date
###########################

meas.hp.trajs.all <-
  bind_rows(
    creatrajs::utils.attach.basemaps(meas.hp.trajs, radius_km = 800, zoom_level = 7) %>% mutate(buffer_km=800)
    # creatrajs::utils.attach.basemaps(meas.hp.trajs, radius_km = 500, zoom_level = 7) %>% mutate(buffer_km=500)
  )
meas.hp.trajs.all$filename=file.path("results","maps","trajs",
                                     sprintf("%s_%s_%s_%dkm.png",
                                             tolower(meas.hp.trajs.all$gadm1_name),
                                             tolower(meas.hp.trajs.all$location_name),
                                             gsub("-","",as.character(meas.hp.trajs.all$date)),
                                             meas.hp.trajs.all$buffer_km))

pbapply::pbmapply(
  creatrajs::map.trajs,
  trajs = meas.hp.trajs.all$trajs,
  basemap = meas.hp.trajs.all$basemap,
  location_id = meas.hp.trajs.all$location_id,
  location_name = meas.hp.trajs.all$location_name,
  date=meas.hp.trajs.all$date,
  meas=list(NULL),
  duration_hour=duration_hour,
  met_type=met_type,
  height=height,
  filename=meas.hp.trajs.all$filename,
  SIMPLIFY=F)


###########################
# Plot by location only
###########################
meas.hp.trajs.grouped <- meas.hp.trajs %>%
  group_by(location_id, location_name, gadm1_name) %>%
  summarise(trajs=list(do.call(bind_rows, trajs))) %>%
  left_join(meas.hp.trajs %>% distinct(location_id, geometry))

lapply(split(meas.hp.trajs.grouped, meas.hp.trajs.grouped$location_id),
       function(m){
         location_id <- unique(m$location_id)
         f <- file.path("results","data","trajectories",
                        paste0("trajs.",location_id,".RDS"))
         saveRDS(m$trajs[[1]],f)
       })


# meas.hp.trajs.grouped <- creatrajs::utils.attach.basemaps(meas.hp.trajs.grouped, radius_km = 2500, zoom_level = 6, source="stamen") %>% mutate(buffer_km=2500)

meas.hp.trajs.grouped$filename <- file.path("results","maps","trajs",
                                     sprintf("%s_%s_%dm.jpg",
                                             tolower(meas.hp.trajs.grouped$gadm1_name),
                                             tolower(meas.hp.trajs.grouped$location_name),
                                             height))


# For debug
map.trajs(
trajs = meas.hp.trajs.grouped$trajs[[1]],
basemap = NULL,
location_id = meas.hp.trajs.grouped$location_id[[1]],
location_name = meas.hp.trajs.grouped$location_name[[1]],
duration_hour=duration_hour,
met_type=met_type,
height=height,
filename=meas.hp.trajs.grouped$filename[[1]])


pbapply::pbmapply(
  map.trajs,
  trajs = meas.hp.trajs.grouped$trajs,
  basemap = list(NULL), # meas.hp.trajs.grouped$basemap,
  location_id = meas.hp.trajs.grouped$location_id,
  location_name = meas.hp.trajs.grouped$location_name,
  duration_hour=duration_hour,
  met_type=met_type,
  height=height,
  plot.width=12,
  plot.height=8,
  filename=meas.hp.trajs.grouped$filename,
  SIMPLIFY=F)
