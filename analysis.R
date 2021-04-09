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

dir.create(file.path("results","data"), recursive=T, showWarnings=F)
dir.create(file.path("results","plots"), recursive=T, showWarnings=F)
dir.create(file.path("results","maps"), recursive=T, showWarnings=F)
dir.create(file.path("results","maps","trajs"), recursive=T, showWarnings=F)
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
meas$sand_storm <- (meas$pm10 > 300) & (meas$pm25/meas$pm10 < 0.75) # THIS AFFECTS RESULTS A LOT
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
png("results/maps/map_change_interpolated.png", width = 1200, height=600)
print(map_change)
dev.off()


# -number of heavy polluted days, excluding sand storms, in the past 12 months up to end of March, by city and province (worst city)
m.hp.city <- meas %>%
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
m.count.city <- meas %>%
  filter(quarter %in% c("2020Q4","2021Q1","2019Q4","2020Q1"),
         !sand_storm,
         heavy_polluted) %>%
  group_by(location_id, province=gadm1_name, city=location_name, quarter) %>%
  summarise(ndays=n()) %>%
  tidyr::spread("quarter","ndays") %>%
  replace(is.na(.), 0)
write.csv(m.count.city, "results/data/heavy_polluted_per_city_quarter.csv", row.names = F)



# back trajectories for the heavy polluted days, for the worst city in each province and for provincial capitals

capitals <- read_csv(file.path("data","cities.csv")) %>%
  filter(capital %in% c("primary","admin")) %>%
  dplyr::select(location_name=city,
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
         !sand_storm,
         date>="2020-04-01") %>%
  group_by(location_id, location_name, gadm1_name) %>%
  summarise(count=n()) %>%
  group_by(gadm1_name) %>%
  filter(count==max(count))

location_ids <- unique(c(capitals$location_id, worse_cities$location_id))

duration_hour=72
met_type="gdas1"
height=500

meas.hp.trajs <- meas %>%
  left_join(rcrea::cities(id=unique(meas$location_id), with_geometry=T) %>% dplyr::select(location_id=id, geometry)) %>%
  filter(location_id %in% location_ids) %>%
  filter(heavy_polluted,
         !sand_storm,
         season=="2020-2021") %>%
  distinct(location_id, location_name, gadm1_name, geometry, date) %>%
  rowwise() %>%
  mutate(
    # meas=list(tibble(pm25=pm25, pm10=pm10))
   )



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




# Old ---------------------------------------------------------------------


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




