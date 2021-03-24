# Udpate packages
install_github("energyandcleanair/rcrea", upgrade=F)
install_github("energyandcleanair/creatrajs", upgrade=F)

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

# Calculate trajectories --------------------------------------------------

meas.hp %>%
  filter(heavy_pollution) %>%
  rowwise() %>%
  mutate(trajs=list(
    creatrajs::trajs.get(
      dates=as.Date(date),
      location_id=location_id,
      geometry=geometry,
      met_type="gdas1",
      heights=500,
      duration_hour=72,
      timezone="Asia/Shanghai",
      cache_folder=file.path("cache","trajs"),
      parallel=F
    )
  ))

