shiny::tabPanel(
  title = "Stock",
  fluidRow(
    column(
      width = 12,
      selectInput("theme_stock", label = "Theme:", choices = c("default", "light", "patterns", "dark", "chalk")),
      br(),
      rAmCharts::amChartsOutput("stock1"),
      verbatimTextOutput("code_stock1")
    )
  )
)