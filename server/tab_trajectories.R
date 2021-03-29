trajs_date <- reactive({
  input$trajs_date
}) %>% debounce(1000)


trajs_location <- reactive({
  req(input$trajs_city)
  req(input$trajs_province)

  trajs_locations() %>%
    dplyr::filter(gadm1_name==input$trajs_province,
                  name==input$trajs_city)
})

trajs_location_id <- reactive({
  req(trajs_location())
  trajs_location() %>% pull(id)
})

trajs_location_geometry <- reactive({
  req(trajs_location())
  trajs_location() %>% pull(geometry)
})

trajs_location_filename <- reactive({
  req(trajs_location())
  trajs_location() %>% pull(filename)
})

trajs <- reactive({
  req(trajs_location_id())

  filename <- trajs_locations() %>%
    dplyr::filter(id==trajs_location_id()) %>%
    dplyr::pull(filename)

  readRDS(filename) %>%
    do.call(bind_rows, .) %>%
    mutate(date=lubridate::date(date))
  #TODO Fix timezone

})

trajs_dates <- reactive({
  req(trajs())
  trajs() %>%
    dplyr::pull(date) %>%
    unique() %>%
    sort(decreasing=T)
})


trajs_meas <- reactive({
  req(trajs_location_id())
  req(meas_locations())

  filename <- meas_locations() %>%
    filter(id==trajs_location_id()) %>%
    pull(filename)

  if(length(filename)==0){
    return(NULL)
  }
  readRDS(filename) %>%
    filter(output=="anomaly") %>%
    tidyr::unnest(normalised)
})

trajs_meas_date <- reactive({

  req(trajs_meas())
  req(trajs_date())

  trajs_meas() %>%
    dplyr::filter(date==trajs_date())

})


trajs_points <- reactive({
  req(trajs())
  req(trajs_date())

  date_ <- tolower(trajs_date())

  tryCatch({
    trajs() %>%
      dplyr::filter(date==date_) %>%
      dplyr::select(date=traj_dt, lon, lat, run)
  }, error=function(e){
    print(sprintf("Failed to read trajectories (%s)",e))
    return(NULL)
  })
})


trajs_plot_poll <- reactive({

  req(trajs_meas())
  req(trajs_date())
  req(input$trajs_running_width)

  poll <- rcrea::poll_str(trajs_meas()$poll[1])
  unit <- trajs_meas()$unit[1]
  hovertemplate <- paste('%{y:.0f}',unit)
  m <- trajs_meas() %>%
    select(date, observed, predicted)
  m.rolled <- rcrea::utils.running_average(m, input$trajs_running_width, vars_to_avg = c("observed","predicted"))


  # selected <- which(trajs_meas_obs()$date==trajs_date())
  m.rolled %>%
    plot_ly(
      type="scatter",
      mode="lines",
      source="trajs"
    ) %>%
    plotly::add_lines(x=~date,
                      y=~observed,
                      name="Observed",
                      opacity=0.4,
                      hovertemplate = hovertemplate,
                      line = list(
                        color = 'rgb(0, 0, 0)',
                        width = 1
                      )) %>%
    plotly::add_lines(x=~date,
                      y=~predicted,
                      name="Predicted",
                      hovertemplate = hovertemplate,
                      line = list(
                        color = 'red',
                        width = 2
                      )) %>%
    plotly::layout(
      showlegend = F,
      hovermode  = 'x unified',
      # title=list(
      #     text=sprintf("%s [%s]",poll, unit),
      #     x=0.1,
      #     font=list(size=10)
      # ),
      yaxis = list(
        # title="", #sprintf("%s [%s]",poll, unit),
        rangemode = 'tozero'
      ),
      xaxis = list(
        title="",
        # showspikes = T,
        spikemode  = 'across+toaxis',
        spikesnap = 'cursor',
        # spikedash = 'solid',
        showline=T,
        showgrid=T
      ),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
      # fig_bgcolor   = "rgba(0, 0, 0, 0)"
    ) %>%
    plotly::add_annotations(
      text = sprintf("%s [%s]",poll, unit),
      x = -0.05,
      y = 1.15,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 12)
    )
})


# Output Elements --------------------------------------
output$selectInputTrajsProvince <- renderUI({
  provinces <- trajs_locations()$gadm1_name %>% unique()
  pickerInput("trajs_province","Province", choices=provinces, options = list(`actions-box` = TRUE), multiple = F)
})

output$selectInputTrajsCity <- renderUI({
  req(input$trajs_province)
  cities <- trajs_locations() %>%
    dplyr::filter(gadm1_name==input$trajs_province) %>%
    dplyr::pull(name) %>%
    unique()
  pickerInput("trajs_city","City", choices=cities, options = list(`actions-box` = TRUE), multiple = F)
})


createInputTrajsDate <- function(value=NULL){
  dates <- trajs_dates()
  if(is.null(value)){
    value <- max(dates)
  }

  # pickerInput("trajs_date","Date", choices=dates, options = list(`actions-box` = TRUE), multiple = F)
  # dateInput("trajs_date","Date", min=min(dates), max=max(dates))
  sliderInput("trajs_date",
              NULL,
              min = as.Date(min(dates),"%Y-%m-%d"),
              max = as.Date(max(dates),"%Y-%m-%d"),
              value=as.Date(value),
              timeFormat="%Y-%m-%d",
              ticks=T,
              width="100%")
}


output$selectInputTrajsDates <- renderUI({
  req(trajs_dates())
  createInputTrajsDate()
})


output$trajsPlots <- renderPlotly({

  req(trajs_plot_poll())

  plots <- list(
    trajs_plot_poll()
  )

  plots <- plots[!is.na(plots)]

  plotly::subplot(plots,
                  nrows = length(plots),
                  shareX = TRUE,
                  titleX = FALSE,
                  titleY = FALSE,
                  shareY = T,
                  margin = 0.05
  ) %>%
    plotly::layout(hovermode='x',
                   xaxis = list(
                     title="",
                     # showspikes = T,
                     spikemode  = 'across+toaxis',
                     spikesnap = 'cursor',
                     # spikedash = 'solid',
                     showline=T,
                     showgrid=T)
    )


})


add_sentinel_layers <- function(map, date){
  for(l in names(sentinel_layers)){
    map <- map %>% addWMSTiles(
      sentinel_url,
      layers = sentinel_layers[[l]],
      layerId = l,
      group = l,
      options = WMSTileOptions(
        tileSize= 512,
        # attribution= '&copy; <a href="http://www.sentinel-hub.com/" target="_blank">Sentinel Hub</a>',
        # urlProcessingApi="https://services.sentinel-hub.com/ogc/wms/aeafc74a-c894-440b-a85b-964c7b26e471",
        # maxcc=20,
        # minZoom:6,
        # maxZoom:16,
        preset=sentinel_layers[[l]],
        # layers:"NO2",
        time=date,
        format = "image/png",
        transparent = F)
    )
  }
  return(map)
}

output$maptrajs <- renderLeaflet({
  map <- leaflet(options = leafletOptions(preferCanvas = TRUE,
                                          zoomControl = FALSE)) %>%
    setView(116,40,6) %>%
    # addProviderTiles(providers$Stamen.TonerLite,
    #                  options = providerTileOptions(noWrap = TRUE)
    # )
    addProviderTiles('Stamen.Terrain', group="Terrain",
                     options=providerTileOptions(zindex=0)) %>%
    addProviderTiles('Esri.WorldImagery', group="Satellite",
                     options=providerTileOptions(zindex=0)) %>%
    addProviderTiles('OpenStreetMap', group = "OpenStreetMap",
                     options=providerTileOptions(zindex=0)) %>%
    # addProviderTiles("CartoDB.PositronOnlyLabels", group="Satellite") %>%
    # addProviderTiles('Esri.Topographic', group="Topographic") %>%
    # addProviderTiles('Esri.Terrain', group="Terrain") %>%
    addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
    addLayersControl(
      baseGroups = c("Terrain", "Satellite", "OpenStreetMap", "Light"),
      overlayGroups = c("Trajectories", "Active fires",
                        names(trajs_gibs_layers),
                        names(sentinel_layers)),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    hideGroup(c(names(trajs_gibs_layers),
                names(sentinel_layers)))


  for(l in names(trajs_gibs_layers)){
    map <- map %>%leaflet.extras2::addGIBS(
      layers=trajs_gibs_layers[[l]],
      group=l,
      dates=lubridate::today(),
      transparent = T,
      opacity = 0.7
    )
  }


  # Add S5P - NO2
  map <- add_sentinel_layers(map, date=lubridate::today())

  return(map)
})


observe({

  req(trajs_date())

  wms_url <- sprintf("https://firms.modaps.eosdis.nasa.gov/wms/key/%s/",Sys.getenv("FIRMS_KEY"))
  wms_layer <- "fires_viirs_snpp"
  leaflet_layer_id <- "firms_wms"
  date_str <- strftime(as.Date(trajs_date()),"%Y-%m-%d")

  #https://firms.modaps.eosdis.nasa.gov/wms/key/YourMapKey/?REQUEST=GetMap&layers=fires_viirs,fires_modis&TIME=2020-01-01/2020-01-10&WIDTH=1024&HEIGHT=512&colors=240+40+40,250+200+50&size=2,2&BBOX=-180,-90,180,90


  leafletProxy("maptrajs") %>%
    removeTiles(leaflet_layer_id) %>%
    leaflet::addWMSTiles(
      wms_url,
      layers = wms_layer,
      layerId = leaflet_layer_id,
      group="Active fires",
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        colors = "255+12+25",
        TIME = date_str,
        size=5,
        zIndex=1000
      )) %>%
    leaflet.extras2::setDate(layers=stack(trajs_gibs_layers)$values,
                             dates=as.Date(trajs_date())) %>%


    # removeTiles(stack(trajs_gibs_layers)$values) %>%
    removeTiles(names(sentinel_layers)) %>%
    add_sentinel_layers(date=date_str)
})

# Incremental changes to the map. Each independent set of things that can change
# should be managed in its own observer.
observe({

  req(trajs_points())
  # req(trajs_fire())

  map <- leafletProxy("maptrajs") %>%
    clearShapes() %>%
    clearMarkers

  trajs <- trajs_points() %>%
    dplyr::arrange(run, date)

  # fires <- trajs_fire()

  for(run in unique(trajs$run)){
    map <- addPolylines(map,
                        lng= ~ lon,
                        lat= ~ lat,
                        data = trajs[trajs$run==run,],
                        group = "Trajectories",
                        weight = 3)
  }

  map
})

observe({

  req(trajs_location_geometry())
  tryCatch({

    leafletProxy("maptrajs") %>%
      setView(
        lng=sf::st_coordinates(trajs_location_geometry())[1],
        lat=sf::st_coordinates(trajs_location_geometry())[2],
        zoom = 6
      )
  }, error=function(e){NULL})

})


# Click date
clickedDate <- reactiveVal()

observe({
  req(trajs_plot_poll)
  d <- unlist(event_data(event = "plotly_click",
                         source = "trajs",
                         priority = "event"))

  if(is.null(d)){return(NULL)}
  clickedDate(d[["x1"]])

  output$selectInputTrajsDates = renderUI(createInputTrajsDate(value=d[["x1"]]))

})

