shinydashboard::tabItem(
  tabName = "bar",
  
  
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
                   rAmCharts::amChartsOutput("bar0"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_bar0"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Color, labal rotation, title", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("bar1"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Color, labal rotation, title", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_bar1"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Group bar plot", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("bar2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Group bar plot", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_bar2"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Stack, chartcursor, custom legend", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("bar3"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Stack, chartcursor, custom legend", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_bar3"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("3D and export", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("bar5"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("3D and export", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_bar5"))
               )
             )
      ),
      tabBox(width=12,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Floating Bar", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("bar4"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Floating Bar", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_bar4"))
               )
             )
      ),
      tabBox(width=12,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Opposition", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("bar6"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Opposition", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_bar6"))
               )
             )
      )
    )
  )
)