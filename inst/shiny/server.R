
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rAmCharts)
library(pipeR)
library(data.table)
library(shinydashboard)
shinyServer(function(input, output) {
  
  category <- reactive({
    "attribute"
  })
  
  data('data_stock1')
  data('data_gdp')
  setnames(data_gdp,"country","label")
  setnames(data_gdp,"gdp","value")
  source("./src/pie/pie_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/pie/AmPie_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/radar/radar_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/radar/amRadar_server.R", local = TRUE, encoding = "UTF-8")
  
  
  source("./src/bar/bar_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/bar/amBar_server.R", local = TRUE, encoding = "UTF-8")
  
  
  
  source("./src/lines/lines_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/lines/amLines_server.R", local = TRUE, encoding = "UTF-8")
  
  
  
  
  # source("./src/serial/serial_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/xy/xy_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/xy/amxy_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./src/stock/stock_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/stock/amstock_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./src/funnel/funnel_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/funnel/amfunnel_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./src/gauge/gauge_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/gauge/amgauge_server.R", local = TRUE, encoding = "UTF-8")
  # source("./src/drilldown/drilldown_server.R", local = TRUE, encoding = "UTF-8")

})
