shiny::tabPanel(
  title = "Funnel",
  fluidRow(
    column(
      width = 12,
      rAmCharts::amChartsOutput("funnel1", type = "funnel"),
      verbatimTextOutput("code_funnel1")
    )
  )
)