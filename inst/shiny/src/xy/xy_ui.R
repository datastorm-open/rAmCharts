shiny::tabPanel(
  title = "XY",
  fluidRow(
    column(
      width = 12,
      selectInput("theme_xy", label = "Theme:", choices = c("default", "light", "patterns", "dark", "chalk")),
      br(),
      rAmCharts::amChartsOutput("xy1", type = "xy"),
      verbatimTextOutput("code_xy1")
    )
  )
)