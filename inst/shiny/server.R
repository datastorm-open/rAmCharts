library(shiny)
library(rAmCharts)
library(pipeR)
library(data.table)
library(shinydashboard)

shinyServer(function(input, output) {
  source("./src/pie/pie_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/pie/amPie_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./src/radar/radar_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/radar/amRadar_server.R", local = TRUE, encoding = "UTF-8")

  source("./src/bar/bar_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/bar/amBar_server.R", local = TRUE, encoding = "UTF-8")

  source("./src/lines/lines_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/lines/amLines_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./src/xy/xy_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/xy/amXY_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./src/stock/stock_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/stock/amStock_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./src/funnel/funnel_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/funnel/amFunnel_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./src/gauge/gauge_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/gauge/amGauge_server.R", local = TRUE, encoding = "UTF-8")

  source("./src/candlestick/candlestick_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/candlestick/amCandlestick_server.R", local = TRUE, encoding = "UTF-8")

  source("./src/bullet/bullet_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/bullet/amBullet_server.R", local = TRUE, encoding = "UTF-8")
  
  source("./src/mekko/mekko_server.R", local = TRUE, encoding = "UTF-8")
  source("./src/mekko/amMekko_server.R", local = TRUE, encoding = "UTF-8")
})
