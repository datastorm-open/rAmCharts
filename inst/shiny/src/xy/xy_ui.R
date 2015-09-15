shiny::tabPanel(
  title = "XY",
  fluidRow(
    column(
      width = 12,
      rAmCharts::amChartsOutput("xy1", type = "xy"),
      verbatimTextOutput("code_xy1")
    )
  )
)