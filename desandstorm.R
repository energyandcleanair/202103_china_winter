library(Metrics)

m <- data.get_meas(use_cache=T, polls=polls, level="city", date_from="2020-01-01", date_to="2021-03-31") %>% left_join(data.keyregions())
# %>% filter(keyregion2018 %in% c("2+26","Fenwei","YRD"))

m.official <- tibble(
  keyregion2018=c(
    "2+26","2+26","2+26",
    "Fenwei","Fenwei","Fenwei",
    "YRD","YRD","YRD"),
  quarter=rep(c("2018Q4","2019Q4","2019Q1"),3),
  pm25.official=c(
    71,63,90,
    62,64,94,
    48,47,61)
)

distance_fn_keyregion <- function(m, m.official, par){

    m.combined <- suppressMessages(m %>% data.enrich_and_widen(storm_pm10_threshold = par[1]*1000,
                                     storm_pm_ratio = par[2]) %>%
      filter(!sand_storm) %>%
      group_by(keyregion2018, quarter) %>%
      summarise(pm25=mean(pm25, na.rm=T)) %>%
      inner_join(m.official))

    mae <- Metrics::mae(m.combined$pm25, m.combined$pm25.official)
    print(sprintf("%f-%f: %f", par[1]*1000, par[2], mae))
    return(mae)
}

optim(par=c(0.3, 0.75),
      fn=distance_fn_keyregion,
      m=m,
      m.official=m.official,
      # method="L-BFGS-B",
      lower=c(0,0),
      upper=c(1,1))



# Shanxi ------------------------------------------------------------------
m <- data.get_meas(use_cache=T, polls=polls, level="city", date_from=date_from, date_to=date_to, years_rel=years_rel) %>% left_join(data.keyregions())
m.shanxi <- m %>%
  data.enrich_and_widen() %>%
  group_by(location_name, province=gadm1_name, year=lubridate::year(date), month=lubridate::month(date)) %>%
  filter(!sand_storm, year==2020) %>%
  summarise(pm25=mean(pm25, na.rm=T)) %>%
  filter(province=="Shanxi")

m.shanxi.count <- m %>%
  data.enrich_and_widen() %>%
  group_by(location_id, province=gadm1_name, year=lubridate::year(date), month=lubridate::month(date)) %>%
  filter(year==2020) %>%
  summarise(count=n()) %>%
  filter(province=="Shanxi")


(m.hebei <- m %>%
  data.enrich_and_widen() %>%
  group_by(location_id, province=gadm1_name, year=lubridate::year(date), month=lubridate::month(date)) %>%
  filter(!sand_storm, year==2020) %>%
  summarise(pm25=mean(pm25, na.rm=T)) %>%
  filter(province=="Hebei"))


m.official.city.shanxi <- tibble(
  location_id=rep(c(
    "taiyuan_chn.25_1_cn",
    "datong_chn.25_1_cn",
    "yangquan_chn.25_1_cn",
    "changzhi_chn.25_1_cn",
    "jincheng_chn.25_1_cn",
    "shuozhou_chn.25_1_cn",
    "jinzhong_chn.25_1_cn",
    "yuncheng_chn.25_1_cn",
    "xinzhou_chn.25_1_cn",
    "linfen_chn.25_1_cn",
    "luliang_chn.25_1_cn"
   ),each=13),
  year=rep(c(rep(2020,12),2021),11),
  month=rep(c(seq(1,12),1),11),
  pm25.official=c(
    117, 77, 45, 43, 36, 44, 37, 28, 33, 59, 71, 56, 56,
    72, 40, 24, 24, 18, 24, 21, 18, 17, 27, 41, 42, 47,
    94, 58, 45, 34, 32, 36, 40, 30, 28, 44, 60, 49, 59,
    108, 47, 36, 37, 26, 38, 37, 26, 34, 48, 50, 46, 61,
    113, 50, 39, 41, 27, 38, 33, 24, 36, 54, 51, 47, 55,
    102, 49, 28, 26, 20, 25, 23, 17, 20, 30, 57, 46, 46,
    100, 55, 34, 31, 25, 35, 33, 24, 27, 41, 54, 46, 43,
    136, 84, 49, 48, 29, 35, 32, 26, 38, 48, 64, 92, 87,
    130, 66, 32, 28, 31, 35, 30, 22, 23, 33, 52, 47, 64,
    159, 69, 37, 36, 23, 35, 30, 26, 36, 52, 57, 64, 75,
    78, 42, 30, 28, 19, 27, 26, 20, 26, 27, 41, 40, 42)
)


m.official.city.beijing <- tibble(
  location_id=rep(c(
    "beijing_chn.2_1_cn"
  ),each=14),
  year=rep(c(rep(2020,12),2021,2021),1),
  month=rep(c(seq(1,12),1,2),1),
  pm25.official=c(59, 63, 35, 31, 36, 32, 41, 29, 24, 41, 38, 29, 38, 63)
)

m.official.city.tianjin <- tibble(
  location_id=rep(c(
    "tianjin_chn.27_1_cn"
  ),each=14),
  year=rep(c(rep(2020,12),2021,2021),1),
  month=rep(c(seq(1,12),1,2),1),
  pm25.official=c(102, 61, 43, 40, 35, 37, 42, 34, 29, 54, 47, 54, 49, 59)
)

m.official.city.hebei <- tibble(
  location_id=rep(c(
  'baoding_chn.10_1_cn',
  'cangzhou_chn.10_1_cn',
  'chengde_chn.10_1_cn',
  'handan_chn.10_1_cn',
  'hengshui_chn.10_1_cn',
  'langfang_chn.10_1_cn',
  'qinhuangdao_chn.10_1_cn',
  'shijiazhuang_chn.10_1_cn',
  'tangshan_chn.10_1_cn',
  'xingtai_chn.10_1_cn',
  'zhangjiakou_chn.10_1_cn'
  ),each=3),
  year=2021,
  month=rep(c(1,2,3),11),
  pm25.official=c(
    56, 71, 73,
    60, 50, 55,
    41, 39, 49,
    70, 62, 52,
    61, 57, 55,
    44, 64, 66,
    50, 59, 62,
    66, 73, 83,
    56, 73, 74,
    66, 65, 63,
    34, 33, 48)
)

distance_fn_city <- function(m, m.official, par){

  m.combined <- suppressMessages(m %>% data.enrich_and_widen(storm_pm10_threshold = par[1]*1000,
                                                             storm_pm_ratio = par[2]) %>%
                                   filter(!sand_storm) %>%
                                   group_by(location_id,
                                            year=lubridate::year(date),
                                            month=lubridate::month(date)) %>%
                                   summarise(pm25=mean(pm25, na.rm=T)) %>%
                                   inner_join(m.official))

  mae <- Metrics::mae(m.combined$pm25, m.combined$pm25.official)
  print(sprintf("%f-%f: %f", par[1]*1000, par[2], mae))
  return(mae)
}

optim(par=c(0.3, 0.75),
      fn=distance_fn_city,
      m=m,
      m.official=rbind(m.official.city.shanxi,
                       m.official.city.hebei,
                       m.official.city.beijing,
                       m.official.city.tianjin)
      )


