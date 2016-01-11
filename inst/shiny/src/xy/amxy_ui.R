shinydashboard::tabItem(
  tabName = "amxy",
  
  
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
                   rAmCharts::amChartsOutput("amxy0"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amxy0"))
               )
             )
      ),
      
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Scrollbar and zoom", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amxy1"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Scrollbar and zoom", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amxy1"))
               )
             )
      ),
      
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Weights, main and color", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amxy2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Weights, main and color", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amxy2"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("add others series", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amxy3"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("add others series", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amxy3"))
               )
             )
      )
      
      
    )
  )
)