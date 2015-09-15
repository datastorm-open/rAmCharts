shiny::tabPanel(
  title = "Gauge",
  fluidRow(
    column(
      width = 12,
      rAmCharts::amChartsOutput("gauge1", type = "gauge"),
      verbatimTextOutput("code_gauge1")
    )
  )
)