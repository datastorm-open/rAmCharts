shiny::tabPanel(
  title = "Pie",
  fluidRow(
    column(
      width = 12,
      h2("Simple example"),
      selectInput("theme_pie", label = "Theme:", choices = c("default", "light", "patterns", "dark", "chalk")),
      br(),
      rAmCharts::amChartsOutput("pie0"),
      verbatimTextOutput("code_pie0"),
      
      h2("Add title and source"),
      rAmCharts::amChartsOutput("pie01"),
      verbatimTextOutput("code_pie01"),
      
      h2("Add legend to select a subset"),
      rAmCharts::amChartsOutput("pie03"),
      verbatimTextOutput("code_pie03"),
      
      h2("Add listener... on legend"),
      rAmCharts::amChartsOutput("pie04"),
      verbatimTextOutput("code_pie04"),
      
      h2("Add listener... on click slice"),
      rAmCharts::amChartsOutput("pie05"),
      verbatimTextOutput("code_pie05"),
      
      h2("Add perspective"),
      rAmCharts::amChartsOutput("pie06"),
      fluidRow(
        column(width = 4,
               align = "center",
               sliderInput("angle_pie", label = "Angle", min = 0, max = 60, value = 30)),
        column(width = 4,
               align = "center",
               sliderInput("depth_pie", label = "Depth", min = 1, max = 25, value = 10)),
        column(width = 4,
               align = "center",
               sliderInput("innerRadius_pie", label = "Inner-Radius", min = 0, max = 80, value = 0))
      ),
      verbatimTextOutput("code_pie06"),
      
      h2("Transform a pie into donut"),
      rAmCharts::amChartsOutput("pie07"),
      verbatimTextOutput("code_pie07")
      
    )
  )
)