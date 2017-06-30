
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


shinyServer(function(session, input, output) {
  
  data <- reactive({
    data_global
  })
  
  res_1 <- callModule(rAmChartTimeSeriesServer, "ts_1", data, "date", "value", main = "Series 1",

                      color = "red", periodFieldsSelection = TRUE, ZoomButton = data.frame(Unit = c("DD", "DD", "MAX"), multiple = c(1, 7 ,1),
                                                              label = c("Day","Week", "MAX"), selected = c(F, F, T)), group = "sh")
  

  output$am1 <- renderAmCharts({
    amTimeSeries(data_stock_2[1:100, ], "date", c("ts1", "ts2"), group = "sh2")
  })
  
  output$am2 <- renderAmCharts({
    amTimeSeries(data_stock_2[1:100, ], "date", c("ts1", "ts2"), group = "sh2")
  })
  
  output$am3 <- renderAmCharts({
    amTimeSeries(data_stock_2[1:100, ], "date", c("ts1", "ts2"), group = "sh2")
  })
  
  output$am4 <- renderAmCharts({
    amTimeSeries(data_stock_2[1:100, ], "date", c("ts1", "ts2"), group = "sh2")
  })
})