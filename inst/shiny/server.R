library(rAmCharts)

library(shiny)
library(shinydashboard)

library(pipeR)
library(data.table)

shinyServer(function(input, output) {
  source("./src/pie/pie_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/pie/ampie_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./src/radar/radar_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/radar/amradar_server.R", local = TRUE, encoding = "UTF-8")

  source("./src/bar/bar_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/bar/ambar_server.R", local = TRUE, encoding = "UTF-8")

  source("./src/lines/lines_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/lines/amlines_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./src/xy/xy_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/xy/amxy_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./src/stock/stock_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/stock/amstock_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./src/funnel/funnel_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/funnel/amfunnel_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./src/gauge/gauge_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/gauge/amgauge_server.R", local = TRUE, encoding = "UTF-8")

  source("./src/candlestick/candlestick_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/candlestick/amcandlestick_server.R", local = TRUE, encoding = "UTF-8")

  source("./src/bullet/bullet_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/bullet/ambullet_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./src/mekko/mekko_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/mekko/ammekko_server.R", local = TRUE, encoding = "UTF-8")
})
