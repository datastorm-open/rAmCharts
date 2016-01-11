shinydashboard::tabItem(
  tabName = "amBullet",
  
  
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
                   rAmCharts::amChartsOutput("amBullet0"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amBullet0"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Color and target", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amBullet1"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Color and target", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amBullet1"))
               )
             )
      ),
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Title and legend", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amBullet2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Title and legend", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amBullet2"))
               )
             )
      )
    )
  )
)