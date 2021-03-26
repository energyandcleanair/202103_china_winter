tabPanel("Summary",
         value="table",
         # class = "no-padding-tab",
         plotlyOutput("plotHpCountProvince", height="calc(100vh - 90px)")  %>% withSpinner(color="#0dc5c1"),
         # DT::dataTableOutput("tableHpCount"),
         div(
           class="row-inline",
           style="width: unset;",
           downloadButton("downloadObservations", "Download observations (PM2.5 & PM10)"),
           downloadButton("downloadDeweathered", "Download deweathered (PM2.5)")
         )

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
