shinydashboard::tabItem(
  tabName = "stock",
  fluidRow(
    column(
      width = 12,
    
      br(),
      rAmCharts::amChartsOutput("stock1"),
      verbatimTextOutput("code_stock1")
    )
  )
)