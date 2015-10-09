shiny::tabPanel(
  title = "Gauge",
  fluidRow(
    column(
      width = 12,
      selectInput("theme_gauge", label = "Theme:", choices = c("default", "light", "patterns", "dark", "chalk")),
      br(),
      rAmCharts::amChartsOutput("gauge1", type = "gauge"),
      verbatimTextOutput("code_gauge1")
    )
  )
)