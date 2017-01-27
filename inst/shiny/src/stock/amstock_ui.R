shinydashboard::tabItem(
  tabName = "amstock",
  
  fluidRow(
    column(
      width = 12,
      h1("Aggregate data", align="center"),
      tabBox(width=12,
         
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 
                 h2("Try zoom, disaggregate data will appear", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amstock1"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 
                 h2("Try zoom, disaggregate data will appear", align="center"),
                 column(
                   width = 12,
                   verbatimTextOutput("code_amstock1")
                 )
               )
             )
      ),
      
      
      tabBox(width=12,
             
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 
                 h2("Style and aggregate period", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amstock2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 
                 h2("Style and aggregate period", align="center"),
                 column(
                   width = 12,
                   verbatimTextOutput("code_amstock2")
                 )
               )
             )
      ),
      tabBox(width=12,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Aggregation type, legend and number max of points before aggregate", align="center"),
                 h4("Group with next graph", align="center"),
                 
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amstock4"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Aggregation type, legend and number max of points before aggregate", align="center"),
                 h4("Group with next graph", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amstock4"))
               )
             )
      ),
      tabBox(width=12,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("More options...", align="center"),
                 h4("Group with previous graph", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amstock3"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("More options...", align="center"),
                 h4("Group with previous graph", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amstock3"))
               )
             )
      )
      
      ,
      h1("Data sets plot"),
      tabBox(width=12,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amstock5"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amstock5"))
               )
             )
      ),
      tabBox(width=12,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Multi pannels", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amstock6"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Multi pannels", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amstock6"))
               )
             )
      ),
      tabBox(width=12,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Others options", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amstock7"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Others options", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amstock7"))
               )
             )
      )
      
      
      
    )
  )
)