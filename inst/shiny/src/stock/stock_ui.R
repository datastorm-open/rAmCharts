shiny::tabPanel(
  title = "Stock",
  fluidRow(
    column(
      width = 12,
      rAmCharts::amChartsOutput("stock1", type = "stock"),
      verbatimTextOutput("code_stock1")
    )
  )
)