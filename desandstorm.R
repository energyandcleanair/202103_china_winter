library(Metrics)

m <- data.get_meas(use_cache=T, polls=polls, level="city", date_from=date_from, date_to=date_to, years_rel=years_rel) %>% left_join(data.keyregions()) %>%
  filter(keyregion2018 %in% c("2+26","Fenwei","YRD"))

distance_fn <- function(m, par){

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
      fn=distance_fn,
      m=m,
      # method="L-BFGS-B",
      lower=c(0,0),
      upper=c(1,1))


