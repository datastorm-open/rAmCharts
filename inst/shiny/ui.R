
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
      rAmCharts::amChartsOutput("radar", type = "radar"),
      rAmCharts::amChartsOutput("radar2", type = "radar"),
      rAmCharts::amChartsOutput("pie", type = "pie"),
      rAmCharts::amChartsOutput("serial", type = "serial"),
      rAmCharts::amChartsOutput("drillColumnChart1", type = "drill"),
      rAmCharts::amChartsOutput("drillColumnChart2", type = "drill")
    )
  )
))
