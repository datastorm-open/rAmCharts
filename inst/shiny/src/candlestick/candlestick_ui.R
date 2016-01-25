shinydashboard::tabItem(
  tabName = "candlesticks",
  
  
  fluidRow(
    column(
      width = 12,
      
      br(),
      
      tabBox(width=12,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("candl0"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_candl0"))
               )
             )
      )
    )
  )
)