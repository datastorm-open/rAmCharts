shinydashboard::tabItem(
  tabName = "amfunnel",
  
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
                   rAmCharts::amChartsOutput("amfunnel1"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Simple example", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_amfunnel1"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Orientation labels, titles", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amfunnel2"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Orientation labels, titles", align="center"),
                 column(
                   width = 12,
                   verbatimTextOutput("code_amfunnel2"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Style", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("amfunnel3"))
               )
             ),
             tabPanel(
               title = "Code",
               fluidRow(
                 column(width = 12,
                        h2("Style", align = "center"),
                        verbatimTextOutput("code_amfunnel3"))
               )
             )
      )
      
      #       tabBox(width = 12, height = 550,
      #              
      #              tabPanel(
      #                title = "Graphic",
      #                fluidRow(
      #                  h2("3D", align="center"),
      #                  column(
      #                    width = 12,
      #                    rAmCharts::amChartsOutput("amfunnel4"))
      #                  
      #                )),
      #              tabPanel(
      #                title = "Code",
      #                fluidRow(
      #                  h2("3D", align="center"),
      #                  column(
      #                    width = 12,
      #                    
      #                    verbatimTextOutput("code_amfunnel4"))
      #                )
      #              )
      #       )
    )
  )
)