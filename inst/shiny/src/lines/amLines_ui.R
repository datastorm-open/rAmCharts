shinydashboard::tabItem(
  tabName = "amLines",
  
  
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
                   rAmCharts::amChartsOutput("amlines0"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amlines0"))
               )
             )
      ),
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Date", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amlines1"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Date", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amlines1"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Multi series, legend", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amlines2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Multi series, legend", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amlines2"))
               )
             )
      )
    )
  )
)