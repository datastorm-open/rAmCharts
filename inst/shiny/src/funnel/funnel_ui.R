shiny::tabPanel(
  title = "Funnel",
  fluidRow(
    column(
      width = 12,
      selectInput("theme_funnel", label = "Theme:", choices = c("default", "light", "patterns", "dark", "chalk")),
      br(),
      rAmCharts::amChartsOutput("funnel1"),
      verbatimTextOutput("code_funnel1")
    )
  )
)