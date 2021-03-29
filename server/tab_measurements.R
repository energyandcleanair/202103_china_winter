









#
#   # selected <- which(trajs_meas_obs()$date==trajs_date())
#   m.rolled %>%
#     plot_ly(
#       type="scatter",
#       mode="lines"
#     ) %>%
#     plotly::add_lines(x=~date,
#                       y=~observed,
#                       name="Observed",
#                       opacity=0.4,
#                       hovertemplate = hovertemplate,
#                       line = list(
#                         color = 'rgb(0, 0, 0)',
#                         width = 2
#                       )) %>%
#     plotly::add_lines(x=~date,
#                       y=~predicted,
#                       name="Predicted",
#                       hovertemplate = hovertemplate,
#                       line = list(
#                         color = 'red',
#                         width = 2
#                       )) %>%
#     plotly::layout(
#       showlegend = F,
#       hovermode  = 'x unified',
#       # title=list(
#       #     text=sprintf("%s [%s]",poll, unit),
#       #     x=0.1,
#       #     font=list(size=10)
#       # ),
#       yaxis = list(
#         # title="", #sprintf("%s [%s]",poll, unit),
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
#       )
#       # plot_bgcolor  = "rgba(0, 0, 0, 0)",
#       # paper_bgcolor = "rgba(0, 0, 0, 0)",
#       # fig_bgcolor   = "rgba(0, 0, 0, 0)"
#     ) %>%
#     plotly::add_annotations(
#       text = sprintf("%s [%s]",poll, unit),
#       x = -0.05,
#       y = 1.15,
#       yref = "paper",
#       xref = "paper",
#       xanchor = "left",
#       yanchor = "top",
#       showarrow = FALSE,
#       font = list(size = 12)
#     )




# Output Elements --------------------------------------

output$selectInputMeasProvince <- renderUI({
  provinces <- meas_locations()$gadm1_name %>% unique()
  pickerInput("meas_province","Province", choices=provinces, options = list(`actions-box` = TRUE), multiple = F)
})

output$measPlots <- renderPlotly({
  req(meas_all())
  req(meas_locations())
  req(input$meas_province)
  req(input$meas_running_width)

  poll <- rcrea::poll_str(meas_all()$poll[1])

  unit <- meas_all()$unit[1]
  hovertemplate <- paste('%{y:.0f}',unit)

  m <- meas_all() %>%
    left_join(meas_locations() %>% select(location_id=id, gadm1_name)) %>%
    filter(gadm1_name==input$meas_province) %>%
    select(location_id, date, observed, predicted, anomaly=value) %>%
    dplyr::left_join(meas_locations() %>% select(location_id=id, location_name=name, province=gadm1_name))

  m.rolled <- rcrea::utils.running_average(m,
                                           input$meas_running_width,
                                           vars_to_avg = c("observed","predicted","anomaly"))


  p <- ggplot(m.rolled %>%
                tidyr::gather("indicator","value",-c(location_id, location_name, province, date)),
              aes(x=date,
                  y=value)) +
    geom_line(aes(color=indicator)) +
    geom_hline(yintercept=0) +
    labs(y=NULL)

  # Divide by day, going horizontally and wrapping with 2 columns
  p <- p + facet_wrap( ~ location_name, ncol=4) + theme_light()

  fig <- ggplotly(p)

  fig
})
