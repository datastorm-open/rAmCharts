shinydashboard::tabItem(
  tabName = "XY",
  fluidRow(
    column(
      width = 12,
     
      br(),
      rAmCharts::amChartsOutput("xy1", type = "xy"),
      verbatimTextOutput("code_xy1")
    )
  )
)