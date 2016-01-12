shinydashboard::tabItem(
  tabName = "xy",
  
  
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
                   rAmCharts::amChartsOutput("xy0"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_xy0"))
               )
             )
      ),
      
      tabBox(width=12,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Two series, custom size bullet, legend", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("xy1"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Two series, custom size bullet, legend", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_xy1"))
               )
             )
      ),
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Error chart", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("xy2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Error chart", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_xy2"))
               )
             )
      )
    )
  )
)