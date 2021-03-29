



# Output Elements --------------------------------------
output$tableHpCountLocation <- DT::renderDataTable({
  req(meas_hp_location())
  d <- meas_hp_location() %>%
    # filter(heavy_pollution) %>%
    mutate(City=factor(location_name),
           Province=factor(gadm1_name),
           Season=factor(season),
           `Heavy Pollution`=factor(as.character(heavy_pollution)),
           Date=strftime(date, "%Y-%m-%d"),
           `PM2.5`=round(pm25),
           `PM10`=round(pm10),
           ) %>%
    select(City, Province, Season, Date, `Heavy Pollution`, `PM2.5`, `PM10`)
  DT::datatable(data=d,
                options =
                  list(
                    pageLength = 25
                  ),
                filter = list(position = 'top', clear=T))

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

