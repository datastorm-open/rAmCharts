shinydashboard::tabItem(
  tabName = "radar",
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
                   rAmCharts::amChartsOutput("radar1"), type = "radar")
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_radar1"))
               )
             )
      ),
      
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Add legend, title, font area color, bullets ....", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("radar3"), type = "radar")
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Add legend, title, font area color, bullets ....", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_radar3"))
               )
             )
      ),
      
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Wind, add guides", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("radar2"), type = "radar")
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Wind, add guides", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_radar2"))
               )
             )
      )
    )
  )
)