
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rAmCharts)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      rAmCharts::amChartOutput("radar", type = "radar"),
      rAmCharts::amChartOutput("radar2", type = "radar"),
      rAmCharts::amChartOutput("pie", type = "pie"),
      rAmCharts::amChartOutput("serial", type = "serial"),
      rAmCharts::amChartOutput("drillColumnChart1", type = "drill"),
      rAmCharts::amChartOutput("drillColumnChart2", type = "drill")
    )
  )
))
