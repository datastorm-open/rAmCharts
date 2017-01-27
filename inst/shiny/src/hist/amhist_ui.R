shinydashboard::tabItem(
  tabName = "amhist",
  
  
  fluidRow(
    column(
      width = 12,
      tabBox(width=12,
             
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amhist1"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   verbatimTextOutput("code_amhist1")
                 )
               )
             )
      ),
      
      tabBox(width=12,
             
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 
                 h2("Color, labels...", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amhist2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 
                 h2("Color, labels...", align="center"),
                 column(
                   width = 12,
                   verbatimTextOutput("code_amhist2")
                 )
               )
             )
      ),
      tabBox(width=12,
             
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 
                 h2("Top labels, density and export", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amhist3"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 
                 h2("Top labels, density and export", align="center"),
                 column(
                   width = 12,
                   verbatimTextOutput("code_amhist3")
                 )
               )
             )
      )
    )
  )
)