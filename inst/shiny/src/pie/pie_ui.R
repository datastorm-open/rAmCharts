shiny::tabPanel(
  title = "Pie",
  fluidRow(
    column(
      width = 12,
      h2("Basic example"),
      rAmCharts::amChartsOutput("pie1", type = "pie"),
      verbatimTextOutput("code_pie1"),
      
      h2("Listeners on legend"),
      rAmCharts::amChartsOutput("pie2", type = "pie"),
      verbatimTextOutput("code_pie2"),
      
      h2(a("Simple radar (API showcase)", href = "http://www.amcharts.com/demos/radar-chart/")),
      rAmCharts::amChartsOutput("pie3", type = "pie"),
      verbatimTextOutput("code_pie3")
    )
  )
)