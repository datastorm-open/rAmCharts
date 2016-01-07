shinydashboard::tabItem(
  tabName = "drilldown",
  fluidRow(
    column(
      width = 12,
      rAmCharts::amChartsOutput("drillColumnChart1", type = "drill"),
      verbatimTextOutput("drillColumnChart1_code")
    )
  )
)