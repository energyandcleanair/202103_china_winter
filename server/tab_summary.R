




meas_observations <- reactive({
  readRDS(file.path("results","data","meas.RDS"))
})

meas_hp_location <- reactive({

  req(meas_observations())

  is_heavy_poll <- function(pm25, pm10, ...){
    (pm25>150) &
      (pm25/pm10 < 0.75)
  }

  meas_observations() %>%
    filter(process_id=="city_day_mad") %>%
    tidyr::spread("poll","value") %>%
    mutate(heavy_pollution=is_heavy_poll(pm25=pm25, pm10=pm10))
})

# Count of heavy polluted days per location and season
meas_hp_count_location <- reactive({
  req(meas_hp_location())

  meas_hp_location() %>%
    group_by(location_id, location_name, gadm1_id, gadm1_name, season) %>%
    summarise(days_polluted=sum(heavy_pollution)) %>%
    filter(days_polluted>0) %>%
    spread("season","days_polluted") %>%
    arrange_at(desc(max(meas()$season)))

})


# Output Elements --------------------------------------
output$tableHpCountLocation <- DT::renderDataTable({
  req(meas_hp_count_location())
  DT::datatable(data=meas_hp_count_location())
})


output$plotHpCountProvince <- renderPlotly({
  req(meas_hp_location())

  meas_hp_province_count <- meas_hp_location() %>%
    filter(heavy_pollution) %>%
    group_by(gadm1_id, gadm1_name, season, date) %>%
    summarise(heavy_pollution=max(heavy_pollution)) %>%
    group_by(gadm1_id, gadm1_name, season) %>%
    summarise(days_polluted=sum(heavy_pollution))


  p <- ggplot(meas_hp_province_count) +
    geom_bar(aes(x=season, y=days_polluted, fill=season),
             stat="identity",
             position="dodge",
             show.legend = F) +
    facet_wrap(~gadm1_name, scales='free_y') +
    theme_light() +
    labs(title="Occurence of heavy polluted days per province",
         y=NULL,
         x=NULL)

  # Divide by day, going horizontally and wrapping with 2 columns
  fig <- ggplotly(p)
  fig
})

output$downloadObservations <- downloadHandler(
  filename = function() {
    "observations.RDS"
  },
  content = function(file) {
    saveRDS(meas_observations(), file)
  }
)

output$downloadDeweathered <- downloadHandler(
  filename = function() {
    "deweathered.RDS"
  },
  content = function(file) {
    saveRDS(meas_all(), file)
  }
)

