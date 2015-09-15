shiny::tabPanel(
  title = "Drilldown",
  fluidRow(
    column(
      width = 12,
      rAmCharts::amChartsOutput("drillColumnChart1", type = "drill"),
      rAmCharts::amChartsOutput("drillColumnChart2", type = "drill")
    )
  )
)