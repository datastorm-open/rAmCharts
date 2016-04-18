shiny::tabPanel(
  title = "Radar",
  fluidRow(
    column(
      width = 12,
      h2("Basic example"),
      selectInput("theme_radar1", label = "theme", choices = c("default", "light", "patterns", "dark", "chalk")),
      br(),
      rAmCharts::amChartsOutput("radar1", type = "radar"),
      verbatimTextOutput("code_radar1"),
      
      h2(a("Simple example (API showcase)", href = "http://www.amcharts.com/demos/polar-chart/")),
      rAmCharts::amChartsOutput("radar2", type = "radar"),
      verbatimTextOutput("code_radar2")
    )
  )
)