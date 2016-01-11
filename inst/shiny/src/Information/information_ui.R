shinydashboard::tabItem(
  tabName = "information",
  h1("rAmCharts"),
  p(strong("rAmCharts"),"is a graphical package based on the javascript library", 
    a("amcharts", href='https://www.amcharts.com/amCharts'),
    "It uses the packages htmlwidgets to generate JS code from R."),
  br(),
  p("You can draw a chart in two different ways:"),
  
  tags$ul(
    tags$li("JS converting: we have based the package on the S4 architecture so that each javascript class 
of the official amcharts API has an equivalent in R.
            You can instantiate the properties using the dedicated constructors and setters."), 
    tags$li("R functions: use the 'am' functions for plotting amcharts (amPlot, amPie, amBoxplot...).
In order to make it easy to plot common charts, we are developing functions that correspond to the usual R syntax.
            Currently, they are not all implemented, you can refer to our dev version on github to stay up-to-date !")
  ),
  
  br(),
  p("For each chart in this app, click on tab 'Complex Function' to display the code converted from javascript or
    click on tab 'Simple Function' to display the R function developed.")
)

