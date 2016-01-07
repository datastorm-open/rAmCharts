shinydashboard::tabItem(
  tabName = "gauge",
  fluidRow(
    column(
      width = 12,
      h2("Theme setter"),
   
      br(),
      rAmCharts::amChartsOutput("gauge1"),
      verbatimTextOutput("code_gauge1"),
      br(), hr(), br(),
      
      h2("Listener on Band"),
      rAmCharts::amChartsOutput("gauge2"),
      verbatimTextOutput("code_gauge2")
    )
  )
)