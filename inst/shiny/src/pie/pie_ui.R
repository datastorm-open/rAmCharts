shiny::tabPanel(
  title = "Pie",
  fluidRow(
    column(
      width = 12,
      h2("Simple example"),
      rAmCharts::amChartsOutput("pie0", type = "pie"),
      verbatimTextOutput("code_pie0"),
      
      h2("Add title and source"),
      rAmCharts::amChartsOutput("pie01", type = "pie"),
      verbatimTextOutput("code_pie01"),
      
      h2("Choose a theme among: light, dark, chalk"),
      rAmCharts::amChartsOutput("pie02", type = "pie"),
      verbatimTextOutput("code_pie02"),
      
      h2("Add legend to select a subset"),
      rAmCharts::amChartsOutput("pie03", type = "pie"),
      verbatimTextOutput("code_pie03"),
      
      h2("Add listener... on legend"),
      rAmCharts::amChartsOutput("pie04", type = "pie"),
      verbatimTextOutput("code_pie04"),
      
      h2("Add listener... on click slice"),
      rAmCharts::amChartsOutput("pie05", type = "pie"),
      verbatimTextOutput("code_pie05"),
      
      h2("Add perspective"),
      rAmCharts::amChartsOutput("pie06", type = "pie"),
      verbatimTextOutput("code_pie06"),
      
      h2("Transform a pie into donut"),
      rAmCharts::amChartsOutput("pie07", type = "pie"),
      verbatimTextOutput("code_pie07")
      
    )
  )
)