shinydashboard::tabItem(
  tabName = "ammekko",
  
  
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
                   rAmCharts::amChartsOutput("ammekko0"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_ammekko0"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Add value and horizontal", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("ammekko1"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Add value and horizontal", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_ammekko1"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Legend", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("ammekko2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Legend", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_ammekko2"))
               )
             )
      )
      
    )
  )
)