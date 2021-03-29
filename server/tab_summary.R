







# Output Elements --------------------------------------
# output$tableHpCountLocation <- DT::renderDataTable({
#   req(meas_hp_count_location())
#   DT::datatable(data=meas_hp_count_location())
# })


output$plotHpCountProvince <- renderPlotly({
  req(meas_hp_location())

  meas_hp_province_count <- meas_hp_location() %>%
    filter(heavy_pollution) %>%
    group_by(gadm1_id, gadm1_name, season, date) %>%
    summarise(heavy_pollution=max(heavy_pollution)) %>%
    group_by(gadm1_id, gadm1_name, season) %>%
    summarise(days_polluted=sum(heavy_pollution))


  p <- ggplot(meas_hp_province_count %>% mutate(season_short=gsub("^20","",gsub("-20","\n",season)))) +
    geom_bar(aes(x=season_short,
                 y=days_polluted,
                 fill=season,
                 text=season),
             stat="identity",
             position="dodge",
             show.legend = F) +
    facet_wrap(~gadm1_name, scales='free_y') +
    theme_light() +
    scale_fill_brewer(palette="Blues")+
    labs(title="Occurence of heavy polluted days in winter season",
         y=NULL,
         x=NULL)

  # Divide by day, going horizontally and wrapping with 2 columns
  fig <- ggplotly(p, tooltip=c("text","y"))
  fig
})


