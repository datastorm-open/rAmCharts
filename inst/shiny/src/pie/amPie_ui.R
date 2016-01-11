shinydashboard::tabItem(
  tabName = "amPie",
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
                   rAmCharts::amChartsOutput("ampie0"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_ampie0"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Change Color", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("ampie1"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Change Color", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_ampie1"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Add legend", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("ampie2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Add legend", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_ampie2"))
               )
             )
      ),
      
      
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("And more (3D, donut, hide value ...)", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("ampie3"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("And more (3D, donut, hide value ...)", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_ampie3"))
               )
             )
      )
      
      
      
    )
  )
)