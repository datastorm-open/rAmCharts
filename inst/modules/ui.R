# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


shinyUI(fluidPage(
  rAmChartsTimeSeriesUI("ts_1"),
  rAmChartsTimeSeriesUI("ts_2"),
  amChartsOutput("am1"),
  amChartsOutput("am2"),
  amChartsOutput("am3"),
  amChartsOutput("am4")
))
