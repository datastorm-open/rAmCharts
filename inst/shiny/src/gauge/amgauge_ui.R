shinydashboard::tabItem(
  tabName = "amgauge",
  
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
                   rAmCharts::amChartsOutput("amangular"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amangular"))
               )
             )
      ),
      
      br(),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Bands", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amangular2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Bands", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amangular2"))
               )
             )
      ),
      
      br(),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Multi bands", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amangular3"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Multi bands", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amangular3"))
               )
             )
      ),
      
      br(),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Solid", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amangular4"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Solid", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amangular4"))
               )
             )
      )
      
      
    )
  )
)