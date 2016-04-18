
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rAmCharts)
library(pipeR)
library(data.table)

shinyServer(function(input, output) {
  
  category <- reactive({
    "attribute"
  })
  
  data('data_stock1')
  data('data_gdp')
  
  source("./src/pie/pie_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/radar/radar_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/serial/serial_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/xy/xy_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/stock/stock_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/gauge/gauge_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/funnel/funnel_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/drilldown/drilldown_server.R", local = TRUE, encoding = "UTF-8")

})
