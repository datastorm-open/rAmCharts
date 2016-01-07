shinydashboard::tabItem(
  tabName = "amRadar",
 
  
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
                   rAmCharts::amChartsOutput("amRadar0"), type = "radar")
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amRadar0"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Add title, legend, export, pch ...", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amRadar1"), type = "radar")
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Add title, legend, export, pch ...", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amRadar1"))
               )
             )
      ),
      
      
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Wind plot, (amWind function)", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amWind"), type = "radar")
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("ind plot, (amWind function)", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amWind"))
               )
             )
      )
      
      
      
    )
  )
)