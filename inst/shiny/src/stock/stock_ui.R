shinydashboard::tabItem(
  tabName = "stock",
  
  fluidRow(
    column(
      width = 12,
      
      br(),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Stock example", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("stock1"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Stock example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_stock1"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Mean if more than 50 points in selected range", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("stock2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Mean if more than 50 points in selected range", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_stock2"))
               )
             )
      )
      
      
    )
  )
  
  
  
)