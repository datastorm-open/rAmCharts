shiny::tabPanel(
  title = "Serial",
  fluidRow(
    column(
      width = 12,
      h2("Theme setter"),
      selectInput("theme_serial", label = "Theme:", choices = c("default", "light", "patterns", "dark", "chalk")),
      br(),
      rAmCharts::amChartsOutput("serial1"),
      verbatimTextOutput("code_serial1"),
      br(), hr(), br(),
      
      h2("3D column chart"),
      rAmCharts::amChartsOutput("serial3"),
      verbatimTextOutput("code_serial3"),
      br(), hr(), br(),
      
      h2("Column and line mix"),
      rAmCharts::amChartsOutput("serial2"),
      verbatimTextOutput("code_serial2"),
      br(), hr(), br(),
      
      h2("Stacked area"),
      rAmCharts::amChartsOutput("serial4"),
      verbatimTextOutput("code_serial4"),
      br(), hr(), br(),
      
      h2("Bi-color smoothed line"),
      rAmCharts::amChartsOutput("serial5"),
      verbatimTextOutput("code_serial5"),
      br(), hr(), br(),
      
      h2("Listener on 'legend'"),
      rAmCharts::amChartsOutput("serial6"),
      verbatimTextOutput("code_serial6"),
      br(), hr(), br(),
      
      h2("Listener on 'categoryAxis'"),
      rAmCharts::amChartsOutput("serial7"),
      verbatimTextOutput("code_serial7"),
      br(), hr(), br(),
      
      h2("Listener on 'chartCursor'"),
      rAmCharts::amChartsOutput("serial8"),
      verbatimTextOutput("code_serial8"),
      # a shiny element to display unformatted text
      br(), hr(), br(),
      
      h2("Listener on 'valueAxes'"),
      rAmCharts::amChartsOutput("serial9"),
      fluidRow(
        column(width = 3, strong("Value clicked on the right axis: ")),
        column(width = 3, verbatimTextOutput("results"))
      ),
      verbatimTextOutput("code_serial9"),
      br(), hr(), br(),
      
      h2("Candle stick with chartScrollbar zoom"),
      rAmCharts::amChartsOutput("serial10")

    )
  )
)