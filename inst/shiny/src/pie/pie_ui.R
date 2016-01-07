shinydashboard::tabItem(
  tabName = "pie",
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
                   rAmCharts::amChartsOutput("pie0"))
                   
        )),
        tabPanel(
          title = "Code",
                   fluidRow(
                     h2("Simple example", align="center"),
                 column(
                   width = 12,
        
                   verbatimTextOutput("code_pie0"))
                 )
        )
        ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Add title and source", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("pie01"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Add title and source", align="center"),
                 column(
                   width = 12,
               
                   verbatimTextOutput("code_pie01"))
               )
             )
      ),
      
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Add legend to select a subset", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("pie03"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Add legend to select a subset", align="center"),
                 column(
                   width = 12,
                 
                   verbatimTextOutput("code_pie03"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Add listener... on legend", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("pie04"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Add listener... on legend", align="center"),
                 column(
                   width = 12,
              
                   verbatimTextOutput("code_pie04"))
               )
             )
      ),
      
      tabBox(width=12,height=550,
             
             tabPanel(
               title = "Graphic",
               fluidRow(
                 h2("Add listener... on click slice", align="center"),
                 column(
                   width = 12,
                   rAmCharts::amChartsOutput("pie05"))
                 
               )),
             tabPanel(
               title = "Code",
               fluidRow(
                 h2("Add listener... on click slice", align="center"),
                 column(
                   width = 12,
                   
                   verbatimTextOutput("code_pie05"))
               )
             )
      )

   
#       ,
#       
#       tabBox(width=12,height=650,
#              
#              tabPanel(
#                title = "Graphic",
#                fluidRow(
#                  h2("Add perspective", align="center"),
#                  column(
#                    width = 12,
#                    rAmCharts::amChartsOutput("pie06")),
#                  column(width = 4,
#                         align = "center",
#                         sliderInput("angle_pie", label = "Angle", min = 0, max = 60, value = 30)),
#                  column(width = 4,
#                         align = "center",
#                         sliderInput("depth_pie", label = "Depth", min = 1, max = 25, value = 10)),
#                  column(width = 4,
#                         align = "center",
#                         sliderInput("innerRadius_pie", label = "Inner-Radius", min = 0, max = 80, value = 0))
#                  
#                )),
#              tabPanel(
#                title = "Code",
#                fluidRow(
#                  h2("Add perspective", align="center"),
#                  column(
#                    width = 12,
#                   
#                    verbatimTextOutput("code_pie06"))
#                )
#              )
#       ),
#       
# 
#       
#       
#       
#       tabBox(width=12,height=550,
#              
#              tabPanel(
#                title = "Graphic",
#                fluidRow(
#                  h2("Transform a pie into donut", align="center"),
#                  column(
#                    width = 12,
#                    rAmCharts::amChartsOutput("pie07"))
#                  
#                )),
#              tabPanel(
#                title = "Code",
#                fluidRow(
#                  h2("Transform a pie into donut", align="center"),
#                  column(
#                    width = 12,
#                   
#                    verbatimTextOutput("code_pie07"))
#                )
#              )
#       )
      
      
    )
  )
)