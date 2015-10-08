# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rAmCharts)

shiny::shinyUI(shiny::navbarPage(
  title = "Examples",
  
  source("./src/pie/pie_ui.R", local = TRUE)$value,
  source("./src/radar/radar_ui.R", local = TRUE)$value,
  source("./src/serial/serial_ui.R", local = TRUE)$value,
  source("./src/xy/xy_ui.R", local = TRUE)$value,
  source("./src/gauge/gauge_ui.R", local = TRUE)$value,
  source("./src/funnel/funnel_ui.R", local = TRUE)$value,
  source("./src/stock/stock_ui.R", local = TRUE)$value,
  source("./src/drilldown/drilldown_ui.R", local = TRUE)$value
  
))
