meas_observations <- reactive({
  obs.mee <- readRDS(file.path("results","data","meas.RDS")) %>%
    tidyr::spread("poll","value") %>%
    filter(location_name != "Beijing")

  (obs.bj <- read.csv("data/aqistudy.beijing.csv") %>%
    select(date, pm25, pm10) %>%
    mutate(location_id="beijing_chn.2_1_cn",
           location_name="Beijing",
           unit="µg/m3",
           source="aqistudy",
           timezone="Asia/Shanghai",
           country="CN",
           gadm1_name="Beijing",
           gadm1_id="chn.2_1") %>%
    mutate(date=as.Date(date, format="%d/%m/%Y"),
           season_year=lubridate::year(date - lubridate::days(5*30)),
           season=paste(season_year, season_year+1,sep="-")) %>%
    select(-c(season_year)))

  bind_rows(obs.mee,
          obs.bj)

})

threshold_pm25 <- reactive({
  input$threshold_pm25
}) %>% debounce(1000)


threshold_pm25_pm10 <- reactive({
  input$threshold_pm25_pm10
}) %>% debounce(1000)


meas_hp_location <- reactive({

  req(meas_observations())
  req(threshold_pm25())
  req(threshold_pm25_pm10())

  is_heavy_poll <- function(pm25, pm10, ...){
    (pm25 >= threshold_pm25()) &
      (pm25/pm10 >= threshold_pm25_pm10())
  }

  meas_observations() %>%
    mutate(heavy_pollution=is_heavy_poll(pm25=pm25, pm10=pm10))
})

# Count of heavy polluted days per location and season
meas_hp_count_location <- reactive({
  req(meas_hp_location())

  meas_hp_location() %>%
    group_by(location_id, location_name, gadm1_id, gadm1_name, season) %>%
    summarise(days_polluted=sum(heavy_pollution)) %>%
    filter(days_polluted>0) %>%
    tidyr::spread("season","days_polluted") %>%
    arrange(desc(`2020-2021`))

})

meas_files <- reactive({

  files <- list.files(deweathered.folder,
                      pattern="meas.*",
                      full.names = T)

  tibble::tibble(
    filename=files,
    location_id=gsub("meas.|.RDS",
                     "",
                     basename(files)))
})

meas_all <- reactive({
  req(meas_files())

  lapply(meas_files()$filename,
         readRDS) %>%
    dplyr::bind_rows(.) %>%
    dplyr::filter(output=="anomaly") %>%
    tidyr::unnest(normalised)
})


meas_locations <- reactive({
  req(meas_files())
  rcrea::locations(id=unique(meas_files()$location_id),
                   level=c("city"),
                   source="mee",
                   with_metadata=T,
                   with_geometry=F,
                   collect=T) %>%
    dplyr::left_join(meas_files(),
                     by=c("id"="location_id"))
})


trajs_files <- reactive({

  files <- list.files(trajs.folder,
                      pattern="trajs.*",
                      full.names = T)

  tibble::tibble(
    filename=files,
    location_id=gsub("trajs.|.RDS",
                     "",
                     basename(files)))
})


trajs_locations <- reactive({
  req(trajs_files())
  rcrea::locations(id=unique(trajs_files()$location_id),
                   level=c("city"),
                   source="mee",
                   with_metadata=T,
                   with_geometry=T,
                   collect=T) %>%
    dplyr::left_join(trajs_files(),
                     by=c("id"="location_id"))
})
