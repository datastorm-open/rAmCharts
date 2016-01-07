shinydashboard::tabItem(
  tabName = "amBar",
  
  
  fluidRow(
    column(
      width = 12,
      
      br(),
      br(),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amBar0"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amBar0"))
               )
             )
      ),
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Color and horizontal", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amBar1"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Color and horizontal", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amBar1"))
               )
             )
      ),
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Group and label", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amBar2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Group and label", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amBar2"))
               )
             )
      ),
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Stack and legend", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amBar3"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Stack and legend", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amBar3"))
               )
             )
      )
      
    )
  )
)