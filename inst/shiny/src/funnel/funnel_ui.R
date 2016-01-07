shinydashboard::tabItem(
  tabName = "funnel",

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
                   rAmCharts::amChartsOutput("funnel1"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_funnel1"))
               )
             )
      ),
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("3D", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("funnel2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("3D", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_funnel2"))
               )
             )
      ),
  
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Revert, labels position ...", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("funnel3"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Revert, labels position ...", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_funnel3"))
               )
             )
      )
    )
  )
)