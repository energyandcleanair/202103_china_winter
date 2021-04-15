library(creadeweather)
library(tidyverse)
library(pbapply)
library(gstat)
library(magrittr)
library(rasterVis)
library(sp)

source('maps.R')
source('utils.R')
source('data.R')

date_from <- "2020-10-01"
date_to <- "2021-03-31"
training_end <- lubridate::date(date_from)-lubridate::years(1)-lubridate::days(1)
training_start <- training_end - lubridate::years(3)

# Get observed measurements and remove sand storms before deweathering
m <- rcrea::measurements(
  date_from=training_start,
  date_to=date_to,
  source="mee",
  poll=c("pm25","pm10"),
  process_id="city_day_mad",
  with_geometry = T) %>%
  data.enrich_and_widen(storm_pm10_threshold = 400,
                        storm_pm_ratio = 0.8)


pbapply::pblapply(split(m, m$location_id),
       function(m){
         tryCatch({
           location_id=unique(m$location_id)
           file.weather <- file.path("results","data","deweathered",sprintf("weather.%s.RDS",location_id))
           file.dew <- file.path("results","data","deweathered",sprintf("meas.%s.RDS",location_id))

           if(file.exists(file.dew) &&
              file.info(file.dew)$size>100){
             return(readRDS(file.dew))
           }else{
             print(location_id)
             m.dew.location <- creadeweather::deweather(
               meas=m[!m$sand_storm & m$poll=="pm25",],
               source="mee",
               poll="pm25",
               output=c("trend"),
               training_start_anomaly = training_start,
               training_end_anomaly = training_end,
               save_weather_filename = file.weather,
               upload_results = F
             )

             saveRDS(m.dew.location, file.dew)
             m.dew.location
           }
         }, error=function(c){return(NA)})
       }) %>% do.call(rbind, .) -> m.dew



saveRDS(m.dew, "results/data/deweathered/m.dew.RDS")
m.dew <- readRDS("results/data/deweathered/m.dew.RDS")


# Deweathered version of meas
meas <- tibble(m.dew) %>%
  dplyr::filter(output=="trend", poll=="pm25") %>%
  tidyr::unnest(normalised) %>%
  left_join(rcrea::cities(id=unique(m.dew$location_id), with_metadata=T) %>%
              dplyr::select(location_id=id, location_name=name, gadm1_id, gadm1_name))

meas <- meas[meas$date >= "2019-01-01",]
meas <- meas %>% tidyr::spread("poll", "value")
# meas$sand_storm <- (meas$pm10 > 300) & (meas$pm25/meas$pm10 < 0.75) # THIS AFFECTS RESULTS A LOT
# meas$heavy_polluted <- (meas$pm25 >= 150) & (meas$pm25/meas$pm10 >= 0.75)
meas$quarter <- as.character(lubridate::quarter(meas$date, with_year=T)) %>% gsub("\\.","Q",.)


#1 % change in PM2.5 concentrations in 2020Q4 and 2021Q1, compared with 2019Q4 and 2019Q1, respectively, with sand storm days eliminated and both observed and deweathered data, by city, province and key region
quarters <- c("2018Q4","2019Q1","2020Q4","2021Q1","2019Q4","2020Q1")
# Province
m.change.province <- meas %>%
  filter(quarter %in% quarters) %>%
  group_by(gadm1_id, province=gadm1_name, quarter) %>%
  summarise_at(c("pm25"),mean, na.rm=T) %>%
  tidyr::spread("quarter","pm25") %>%
  mutate(
    `2021Q1_vs_2019Q1_str`=paste0(round(`2021Q1`/`2019Q1`-1,3)*100,"%"), # For csv
    `2020Q4_vs_2019Q4_str`=paste0(round(`2020Q4`/`2019Q4`-1,3)*100,"%"), # For csv
    `2021Q1_vs_2019Q1`=`2021Q1`/`2019Q1`-1, # For map
    `2020Q4_vs_2019Q4`=`2020Q4`/`2019Q4`-1 # For map
    )

write.csv(m.change.province, "results/data/change_province_deweathered.csv", row.names = F)
map.change_province(m.change.province, folder="results/maps/deweathered/")


# City
m.change.city <- meas %>%
  filter(quarter %in% quarters) %>%
  group_by(location_id, province=gadm1_name, city=location_name, quarter) %>%
  summarise_at(c("pm25"), mean, na.rm=T) %>%
  tidyr::spread("quarter","pm25") %>%
  mutate(
    `2021Q1_vs_2019Q1_str`=paste0(round(`2021Q1`/`2019Q1`-1,3)*100,"%"), # For csv
    `2020Q4_vs_2019Q4_str`=paste0(round(`2020Q4`/`2019Q4`-1,3)*100,"%"), # For csv
    `2021Q1_vs_2019Q1`=`2021Q1`/`2019Q1`-1, # For map
    `2020Q4_vs_2019Q4`=`2020Q4`/`2019Q4`-1 # For map
  )

write.csv(m.change.city, "results/data/change_city_deweathered.csv", row.names = F)
map.change_city(m.change.city)

# Key regions
m.change.keyregion <- meas %>%
  rename(province=gadm1_name) %>%
  left_join(data.keyregions()) %>%
  filter(quarter %in% quarters) %>%
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

write.csv(m.change.keyregion, "results/data/change_keyregion_deweathered.csv", row.names = F)


# Map change
map_change <- map.change_interpolated(meas %>% mutate(sand_storm=F), res=0.1)
png("results/maps/map_change_interpolated_deweathered.png", width = 1200, height=600)
print(map_change)
dev.off()


