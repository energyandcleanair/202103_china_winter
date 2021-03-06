


meas_location <- reactive({
  req(meas_locations())
  req(input$province)
  req(input$city)

  meas_locations() %>%
    dplyr::filter(name==input$city,
                  gadm1_name==input$province)
})

meas_location_id <- reactive({
  req(meas_location())
  meas_location()
    dplyr::pull(id)
})


meas_location_geometry <- reactive({
  req(meas_location())
  meas_location() %>%
    dplyr::pull(geometry)
})


meas <- reactive({
  req(meas_location())
  m <- readRDS(meas_location() %>% dplyr::pull(filename))
  m %>%
    filter(output=="counterfactual") %>%
    tidyr::unnest(normalised)
})

#
# trajs_dates <- reactive({
#   req(trajs())
#   trajs() %>%
#     dplyr::pull(date) %>%
#     unique() %>%
#     sort(decreasing=T)
# })

# trajs_fires_all <- reactive({
#     req(trajs_location_id())
#
#     gcs_url <- paste0(trajs.bucket_base_url,
#                       trajs.folder,"/",
#                       trajs_location_id(),
#                       ".fires.RDS")
#     read_gcs_url(gcs_url)
# })

weather <- reactive({
  filename <- gsub("meas.",
                   "weather.",
                   meas_location() %>% pull(filename))

  readRDS(filename) %>%
    select(meas_weather) %>%
    tidyr::unnest(cols=c(meas_weather))
})

# trajs_meas_all <- reactive({
#   req(trajs_location_id())
#   gcs_url <- paste0(trajs.bucket_base_url,
#                     trajs.folder,"/",
#                     trajs_location_id(),
#                     ".meas.RDS")
#   read_gcs_url(gcs_url)
# })
#
# trajs_meas_date <- reactive({
#
#   req(trajs_meas_all())
#   req(trajs_date())
#
#   trajs_meas_all() %>%
#     dplyr::filter(date==trajs_date())
#
# })

# trajs_meas <- reactive({
#     req(trajs_location_id())
#     gcs_url <- paste0(trajs.bucket_base_url,
#                       trajs.folder,"/",
#                       trajs_location_id(),
#                       ".dew.RDS")
#     tryCatch({
#         m.dew <-  readRDS(url(gcs_url))
#
#         # Get observations
#         rcrea::measurements(location_id=trajs_location_id(),
#                             date_from="2017-01-01",
#                             deweathered = F,
#                             # process_id="city_day_mad",
#                             source=m.dew$source[1],
#                             poll=m.dew$poll[1])
#     }, error=function(c){
#         return(NULL)
#     })
# })

# trajs_plot_url <- reactive({
#     date_ <- tolower(trajs_date())
#     city_ <- tolower(input$trajs_city)
#     country_ <- tolower(input$trajs_country)
#     req(date_, country_, city_)
#
#     url.short <- trajs_files() %>%
#         dplyr::filter(tolower(country)==country_,
#                        date==date_,
#                        tolower(city)==city_) %>%
#         dplyr::pull(name) %>% as.character()
#     paste("https://storage.googleapis.com", trajs.bucket, url.short, sep="/")
#
# })
#
# trajs_points <- reactive({
#   req(trajs())
#   req(trajs_date())
#
#   date_ <- tolower(trajs_date())
#
#   tryCatch({
#     trajs() %>%
#       dplyr::filter(date==date_) %>%
#       tidyr::unnest(trajs, names_sep=".") %>%
#       dplyr::select(date=trajs.traj_dt, lon=trajs.lon, lat=trajs.lat, run=trajs.run)
#   }, error=function(e){
#     trajs_add_log(sprintf("Failed to read trajectories (%s)",e))
#     return(NULL)
#   })
# })

meas_plot <- reactive({

  req(meas())
  req(weather())
  req(input$running_width)

  poll <- rcrea::poll_str(meas()$poll[1])
  unit <- meas()$unit[1]
  hovertemplate <- paste('%{y:.0f}',unit)

  m <- meas() %>%
    select(date, observed, predicted)

  m.rolled <- rcrea::utils.running_average(m,
                                           input$running_width,
                                           vars_to_avg = c("observed","predicted"))


  # selected <- which(trajs_meas_obs()$date==trajs_date())
  m.rolled %>%
    plot_ly(
      type="scatter",
      mode="lines"
    ) %>%
    plotly::add_lines(x=~date,
                      y=~observed,
                      name="Observed",
                      opacity=0.4,
                      hovertemplate = hovertemplate,
                      line = list(
                        color = 'rgb(0, 0, 0)',
                        width = 2
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
      )
      # plot_bgcolor  = "rgba(0, 0, 0, 0)",
      # paper_bgcolor = "rgba(0, 0, 0, 0)",
      # fig_bgcolor   = "rgba(0, 0, 0, 0)"
    ) %>%
    plotly::add_annotations(
      text = sprintf("%s [%s]",poll, unit),
      x = 0.5,
      y = 1.05,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "top",
      showarrow = FALSE,
      font = list(size = 12)
    )
})


weather_plots <- reactive({
  req(weather())
  req(input$running_width)

  hovertemplate <- paste('%{y:.0f}')
  cols <- c("air_temp","wd","ws","precip","RH","pbl_min","pbl_max")

  w <- weather() %>%
    select_at(c("date", cols))



  w.rolled <- rcrea::utils.running_average(w,
                                           input$running_width,
                                           vars_to_avg = cols)


  lapply(cols,
         function(col){
           w.rolled %>%
             select(date, col) %>%
             rename(value=col) %>%
             plot_ly(
               type="scatter",
               mode="lines"
             ) %>%
             plotly::add_lines(x=~date,
                               y=~value,
                               name=col,
                               opacity=0.4,
                               hovertemplate = hovertemplate,
                               line = list(
                                 color = 'rgb(0, 0, 0)',
                                 width = 2
                               )) %>%
             plotly::layout(
               showlegend = F,
               hovermode  = 'x unified',
               # title=list(
               #     text=sprintf("%s",col),
               #     x=0.1,
               #     font=list(size=10)
               # ),
               yaxis = list(
                 # title="", #sprintf("%s [%s]",poll, unit),
                 rangemode = 'tozero'
               ),
               xaxis = list(
                 title="",
                 spikemode  = 'across+toaxis',
                 spikesnap = 'cursor',
                 showline=T,
                 showgrid=T
               )
             ) %>%
             plotly::add_annotations(
               text = sprintf("%s",col),
               x = 0.5,
               y = 1.05,
               yref = "paper",
               xref = "paper",
               xanchor = "middle",
               yanchor = "top",
               showarrow = FALSE,
               font = list(size = 12)
             )
         })

})
#
# trajs_plot_fire <- reactive({
#
#   req(trajs_weather())
#   req(input$trajs_running_width)
#
#   f <- trajs_weather() %>%
#     dplyr::select(date, value=fire_count)
#
#   f.rolled <- rcrea::utils.running_average(f, input$trajs_running_width)
#
#   # selected <- which(trajs_meas_obs()$date==trajs_date())
#   f.rolled %>%
#     plot_ly(
#       x = ~date,
#       y = ~value
#       # selectedpoints=as.list(selected),
#     ) %>%
#     plotly::add_lines(name="Fire count") %>%
#     plotly::layout(
#       # title=list(
#       #     text="Fire count (within 10km of trajectories)",
#       #     x=0.1,
#       #     font=list(size=10)
#       # ),
#       showlegend = F,
#       hovermode  = 'x unified',
#       yaxis = list(
#         title="", #Fire count (within 10km of trajectories)",
#         rangemode = 'tozero'
#       ),
#       xaxis = list(title="")) %>%
#     plotly::add_annotations(
#       text = "Fire count (within 10km of trajectories)",
#       x = -0.05,
#       y = 1.15,
#       yref = "paper",
#       xref = "paper",
#       xanchor = "left",
#       yanchor = "top",
#       showarrow = FALSE,
#       font = list(size = 12)
#     )
# })
#
# trajs_plot_firecontribution <- reactive({
#
#   req(trajs_meas_all())
#   req(trajs_date())
#   req(input$trajs_running_width)
#
#   poll <- rcrea::poll_str(trajs_meas_all()$poll[1])
#   unit <- trajs_meas_all()$unit[1]
#   hovertemplate <- paste('%{y:.0f}',unit)
#   m <- trajs_meas_all()     %>%
#     select(date, observed, predicted, predicted_nofire) %>%
#     mutate(value=predicted-predicted_nofire) %>%
#     select(date, value)
#
#   m.rolled <- rcrea::utils.running_average(m, input$trajs_running_width)
#
#
#   # selected <- which(trajs_meas_obs()$date==trajs_date())
#   m.rolled %>%
#     plot_ly(
#       type="scatter",
#       mode="lines"
#     ) %>%
#     plotly::add_lines(x=~date,
#                       y=~value,
#                       hovertemplate = hovertemplate,
#                       line = list(
#                         width = 2
#                       )) %>%
#     plotly::layout(
#       # title=list(
#       #     text=sprintf("Fire contribution to %s [%s]",poll, unit),
#       #     x=0.1,
#       #     font=list(size=10)
#       #     ),
#       showlegend = F,
#       hovermode  = 'x unified',
#       yaxis = list(
#         # title=sprintf("Fire contribution to %s [%s]",poll, unit),
#         rangemode = 'tozero'
#       ),
#       xaxis = list(
#         title="",
#         # showspikes = T,
#         spikemode  = 'across+toaxis',
#         spikesnap = 'cursor',
#         # spikedash = 'solid',
#         showline=T,
#         showgrid=T
#       ),
#       plot_bgcolor  = "rgba(0, 0, 0, 0)",
#       paper_bgcolor = "rgba(0, 0, 0, 0)",
#       fig_bgcolor   = "rgba(0, 0, 0, 0)"
#     ) %>%
#     plotly::add_annotations(
#       text = sprintf("Fire contribution to %s [%s]",poll, unit),
#       x = -0.05,
#       y = 1.15,
#       yref = "paper",
#       xref = "paper",
#       xanchor = "left",
#       yanchor = "top",
#       showarrow = FALSE,
#       font = list(size = 12)
#     )
# })

# trajs_buffer <- reactive({
#     req(trajs())
#     req(trajs_date())
#
#     date_ <- tolower(trajs_date())
#
#     creatrajs::trajs.buffer(trajs()[trajs()$date==date_,"trajs"][[1]][[1]],
#                             buffer_km=10)
# })
#


# Download
# output$trajs_download_jpg <- downloadHandler(
#     # filename = function() {
#     #     paste("trajectories.jpg", sep = "")
#     # },
#     # content = function(file) {
#     #     write.csv(exc(), file, row.names = FALSE)
#     # }
# )


# Output Elements --------------------------------------
output$selectProvince <- renderUI({
  req(meas_locations())
  print("Select province")

  pickerInput("province","Province",
              choices=unique(meas_locations()$gadm1_name),
              options = list(`actions-box` = TRUE), multiple = F)
})

output$selectCity <- renderUI({
  req(meas_locations())
  req(input$province)
  cities <- meas_locations() %>%
    dplyr::filter(gadm1_name==input$province) %>%
    dplyr::pull(name) %>%
    unique()
  pickerInput("city","City",
              choices=cities,
              options = list(`actions-box` = TRUE), multiple = F)
})

# output$selectInputTrajsDates <- renderUI({
#   dates <- trajs_dates()
#   # pickerInput("trajs_date","Date", choices=dates, options = list(`actions-box` = TRUE), multiple = F)
#   # dateInput("trajs_date","Date", min=min(dates), max=max(dates))
#   sliderInput("trajs_date",
#               NULL,
#               min = as.Date(min(dates),"%Y-%m-%d"),
#               max = as.Date(max(dates),"%Y-%m-%d"),
#               value=as.Date(max(dates)),
#               timeFormat="%Y-%m-%d",
#               ticks=T,
#               width="100%")
# })

# output$trajsLogs <- renderText({
#     req(trajs_logs)
#     trajs_logs[["log"]]
#     })
#
# output$trajsInfos <- renderUI({
#   req(trajs_location_id())
#   req(trajs_date())
#   req(trajs_meas_date())
#
#   l <- trajs_locations() %>%
#     dplyr::filter(id==trajs_location_id())
#   d <- trajs_meas_date()
#
#   HTML(paste0("<b>",l$name," - ",d[["poll"]],"</b><br/>",
#               trajs_date(),"<br/>",
#               "Observed: ", round(d[["observed"]]), " ",d[["unit"]], "<br/>",
#               "Predicted: ", round(d[["predicted"]]), " ",d[["unit"]], "<br/>",
#               "Predicted (nofire): ", round(d[["predicted_nofire"]]), " ",d[["unit"]], "<br/>"
#   ))
# })
clickData <- reactiveVal()

observe({
  clickData(unlist(event_data(event = "plotly_click", priority = "event")))
  print(clickData())
})



output$plots <- renderPlotly({

  req(meas_plot())
  req(weather_plots())

  plots <- c(
    list(meas_plot()),
    weather_plots()
  )


  plots <- plots[!is.na(plots)]

  plotly::subplot(plots,
                  nrows = floor(length(plots)/3),
                  shareX = T,
                  titleX = FALSE,
                  titleY = FALSE,
                  shareY = F,
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

#
# add_sentinel_layers <- function(map, date){
#   for(l in names(sentinel_layers)){
#     map <- map %>% addWMSTiles(
#       sentinel_url,
#       layers = sentinel_layers[[l]],
#       layerId = l,
#       group = l,
#       options = WMSTileOptions(
#         tileSize= 512,
#         # attribution= '&copy; <a href="http://www.sentinel-hub.com/" target="_blank">Sentinel Hub</a>',
#         # urlProcessingApi="https://services.sentinel-hub.com/ogc/wms/aeafc74a-c894-440b-a85b-964c7b26e471",
#         # maxcc=20,
#         # minZoom:6,
#         # maxZoom:16,
#         preset=sentinel_layers[[l]],
#         # layers:"NO2",
#         time=date,
#         format = "image/png",
#         transparent = F)
#     )
#   }
#   return(map)
# }
#
# output$maptrajs <- renderLeaflet({
#   map <- leaflet(options = leafletOptions(preferCanvas = TRUE,
#                                    zoomControl = FALSE)) %>%
#     setView(80,30,6) %>%
#     # addProviderTiles(providers$Stamen.TonerLite,
#     #                  options = providerTileOptions(noWrap = TRUE)
#     # )
#     addProviderTiles('Stamen.Terrain', group="Terrain",
#                      options=providerTileOptions(zindex=0)) %>%
#     addProviderTiles('Esri.WorldImagery', group="Satellite",
#                      options=providerTileOptions(zindex=0)) %>%
#     addProviderTiles('OpenStreetMap', group = "OpenStreetMap",
#                      options=providerTileOptions(zindex=0)) %>%
#     # addProviderTiles("CartoDB.PositronOnlyLabels", group="Satellite") %>%
#     # addProviderTiles('Esri.Topographic', group="Topographic") %>%
#     # addProviderTiles('Esri.Terrain', group="Terrain") %>%
#     addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
#     addLayersControl(
#       baseGroups = c("Terrain", "Satellite", "OpenStreetMap", "Light"),
#       overlayGroups = c("Trajectories", "Active fires",
#                         names(trajs_gibs_layers),
#                         names(sentinel_layers)),
#       options = layersControlOptions(collapsed = FALSE)
#     ) %>%
#     hideGroup(c(names(trajs_gibs_layers),
#                  names(sentinel_layers)))
#
#
#   for(l in names(trajs_gibs_layers)){
#     map <- map %>%leaflet.extras2::addGIBS(
#       layers=trajs_gibs_layers[[l]],
#       group=l,
#       dates=lubridate::today(),
#       transparent = T,
#       opacity = 0.7
#     )
#   }
#
#
#   # Add S5P - NO2
#   map <- add_sentinel_layers(map, date=lubridate::today())
#
#
#   # sentinelHub = L.tileLayer.wms(baseUrl, {
#   #   tileSize: 512,
#   #   attribution: '&copy; <a href="http://www.sentinel-hub.com/" target="_blank">Sentinel Hub</a>',
#   #   urlProcessingApi:"https://services.sentinel-hub.com/ogc/wms/aeafc74a-c894-440b-a85b-964c7b26e471",
#   #   maxcc:20,
#   #   minZoom:6,
#   #   maxZoom:16,
#   #   preset:"NO2",
#   #   layers:"NO2",
#   #   time:"2020-09-01/2021-03-17",
#   #
#   # });
#
#   return(map)
# })

#
# observe({
#
#   req(trajs_date())
#
#   wms_url <- sprintf("https://firms.modaps.eosdis.nasa.gov/wms/key/%s/",Sys.getenv("FIRMS_KEY"))
#   wms_layer <- "fires_viirs_snpp"
#   leaflet_layer_id <- "firms_wms"
#   date_str <- strftime(as.Date(trajs_date()),"%Y-%m-%d")
#
#   #https://firms.modaps.eosdis.nasa.gov/wms/key/YourMapKey/?REQUEST=GetMap&layers=fires_viirs,fires_modis&TIME=2020-01-01/2020-01-10&WIDTH=1024&HEIGHT=512&colors=240+40+40,250+200+50&size=2,2&BBOX=-180,-90,180,90
#
#
#   leafletProxy("maptrajs") %>%
#     removeTiles(leaflet_layer_id) %>%
#     leaflet::addWMSTiles(
#       wms_url,
#       layers = wms_layer,
#       layerId = leaflet_layer_id,
#       group="Active fires",
#       options = WMSTileOptions(
#         format = "image/png",
#         transparent = TRUE,
#         colors = "255+12+25",
#         TIME = date_str,
#         size=5,
#         zIndex=1000
#       )) %>%
#       leaflet.extras2::setDate(layers=stack(trajs_gibs_layers)$values,
#                                dates=as.Date(trajs_date())) %>%
#
#
#     # removeTiles(stack(trajs_gibs_layers)$values) %>%
#     removeTiles(names(sentinel_layers)) %>%
#     add_sentinel_layers(date=date_str)
# })
#
# # Incremental changes to the map. Each independent set of things that can change
# # should be managed in its own observer.
# observe({
#
#   req(trajs_points())
#   # req(trajs_fire())
#
#   map <- leafletProxy("maptrajs") %>%
#     clearShapes() %>%
#     clearMarkers
#
#   trajs <- trajs_points() %>%
#     dplyr::arrange(run, date)
#
#   # fires <- trajs_fire()
#
#   for(run in unique(trajs$run)){
#     map <- addPolylines(map,
#                         lng= ~ lon,
#                         lat= ~ lat,
#                         data = trajs[trajs$run==run,],
#                         group = "Trajectories",
#                         weight = 3)
#   }
#
#   # if(!is.null(fires)){
#   #     map <- addCircleMarkers(map,
#   #                             radius=5,
#   #                             color="green",
#   #                             stroke=F,
#   #                             opacity=0.7,
#   #                             group = "Active fires",
#   #                             data = fires)
#   # }
#
#   map
# })
#
# observe({
#
#   req(trajs_location_geometry())
#   tryCatch({
#
#     leafletProxy("maptrajs") %>%
#       setView(
#         lng=sf::st_coordinates(trajs_location_geometry())[1],
#         lat=sf::st_coordinates(trajs_location_geometry())[2],
#         zoom = 6
#       )
#   }, error=function(e){NULL})
#
#
#   # req(trajs())
#   #
#   # tryCatch({
#   #     t<-trajs() %>%
#   #         tidyr::unnest(trajs, names_sep=".") %>%
#   #         dplyr::select(date=trajs.traj_dt, lon=trajs.lon, lat=trajs.lat, run=trajs.run)
#   #
#   #     buffer <- 0.5
#   #     leafletProxy("maptrajs") %>%
#   #         fitBounds(max(t$lon) + buffer,
#   #                   max(t$lat) + buffer,
#   #                   min(t$lon) - buffer,
#   #                   min(t$lat) - buffer)
#   # }, error=function(e){NULL})
# })
#



# observe(trajs_logs_raw, {
#     trajs_logs(trajs_logs_raw)
#     })




# output$imageTrajs <- renderUI({
#     imgurl <- trajs_plot_url()
#     # tags$img(src=imgurl[1], height=800) #TODO account for various met_types
#
#     image_output_list <-
#         lapply(1:length(imgurl),
#                function(i)
#                {
#                    tags$img(src=imgurl[i], height=800)
#                    # imagename = i
#                    # imageOutput(imagename)
#                })
#
#     do.call(tagList, image_output_list)
# })
