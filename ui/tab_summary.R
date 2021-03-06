tabPanel("Summary",
         value="summary",
         div(
           class="row-inline",
           height=50,
           numericInput(
             "threshold_pm25",
             "PM2.5 >=",
             150,
             min = 0,
             max = 1000
           ),
           numericInput(
             "threshold_pm25_pm10",
             "PM2.5 / PM10 >=",
             0.75,
             min = 0,
             max = 1000
           )
         ),
         # class = "no-padding-tab",
         plotlyOutput("plotHpCountProvince", height="calc(100vh - 110px)")  %>% withSpinner(color="#0dc5c1")

)
# tabPanel("Trajectories", value="trajectories", fluid = TRUE,
#          sidebarLayout(
#              sidebarPanel(
#                  width = 2,
#                  uiOutput("selectInputTrajsCountry"),
#                  uiOutput("selectInputTrajsCity"),
#                  uiOutput("selectInputTrajsDates")
#
#                  # downloadButton("trajs_download_jpg", "Download (.jpg)"),
#              ),
#
#              mainPanel(
#                  width=10,
#                  uiOutput("imageTrajs")  %>% withSpinner(color="#0dc5c1")
#                  # plotOutput("exc_status_map"),
#                  # DT::dataTableOutput("exc_status_table")
#                  # DT::dataTableOutput("trajs_table")
#              )
#          )
# )
