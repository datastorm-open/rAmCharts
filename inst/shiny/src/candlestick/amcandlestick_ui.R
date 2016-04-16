shinydashboard::tabItem(
  tabName = "amcandlesticks",
  
  
  fluidRow(
    column(
      width = 12,
      
      br(),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("cds0"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_cds0"))
               )
             )
      ),
      
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("scrollbar and horizontal", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("cds1"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("scrollbar and horizontal", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_cds1"))
               )
             )
      ),
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("By month", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("cds2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("By month", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_cds2"))
               )
             )
      )
    )
  )
)