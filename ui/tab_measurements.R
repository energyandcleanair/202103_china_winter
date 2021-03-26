tabPanel("Measurements",
         value="measurements",
         class = "no-padding-tab",
         sidebarLayout(
           mainPanel(
             width=10,
             plotlyOutput("measPlots", height='calc(100vh - 60px)') %>% withSpinner(color="#0dc5c1")
           ),
           sidebarPanel(
             width = 2,
             # div(
             #   class="row-inline",
             #   height=50,
             #   uiOutput("selectMeasProvince"),
             #   uiOutput("selectCity")
             # ),

             sliderInput("meas_running_width", "Rolling average (day)", min=1, max=30, value=1, step=1, sep = "")
             # verbatimTextOutput("trajsLogs", placeholder = TRUE)

           )

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
