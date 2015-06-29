
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rAmChart)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      rAmChart::amChartOutput("radar", type = "radar"),
      rAmChart::amChartOutput("radar2", type = "radar"),
      rAmChart::amChartOutput("pie", type = "pie"),
      rAmChart::amChartOutput("serial", type = "serial"),
      rAmChart::amChartOutput("drillColumnChart1", type = "drill"),
      rAmChart::amChartOutput("drillColumnChart2", type = "drill")
    )
  )
))
