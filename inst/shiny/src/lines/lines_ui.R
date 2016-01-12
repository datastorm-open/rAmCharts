shinydashboard::tabItem(
  tabName = "lines",
  
  
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
                   rAmCharts::amChartsOutput("lines0"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_lines0"))
               )
             )
      ),
      
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Info bar, bullet and zoom", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("lines1"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Info bar, bullet and zoom", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_lines1"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Avrage line, change color of negative points and smooth lines", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("lines2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Avrage line, change color of negative points and smooth lines", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_lines2"))
               )
             )
      ),
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("confidence range", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("lines3"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("confidence range", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_lines3"))
               )
             )
      ),
      tabBox(width=12,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("confidence range, multi lines, legend", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("lines4"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("confidence range, multi lines, legend", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_lines4"))
               )
             )
      ),
      tabBox(width=12,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Area, guides, axe title, image in bullet ...", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("lines5"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Area, guides, axe title, image in bullet ...", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_lines5"))
               )
             )
      )
      
      
      
      
      
      
    )
  )
)