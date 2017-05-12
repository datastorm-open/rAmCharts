
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


shinyServer(function(session, input, output) {
  
  res_1 <- callModule(rAmChartTimeSeriesServer, "ts_1", data, "date", c("value", "value2"), main = "Series 1",
                      color = "red", periodFieldsSelection = TRUE, ZoomButton = data.frame(Unit = c("DD", "DD", "MAX"), multiple = c(1, 7 ,1),
                                                              label = c("Day","Week", "MAX"), selected = c(F, F, T)), group = "sh")
  
  observe({
    print(str(res_1()))
  })

  # res_2 <- callModule(rAmChartTimeSeriesServer, "ts_2", data, "date", "value", periodFieldsSelection = TRUE, group = "sh")
  # 
  # observe({
  #   print(str(res_2()))
  # })
  # 
  output$am1 <- renderAmCharts({
    data("data_stock_2")
    amTimeSeries(data_stock_2[1:100, ], "date", c("ts1", "ts2"), group = "sh2")
  })
  
  output$am2 <- renderAmCharts({
    data("data_stock_2")
    amTimeSeries(data_stock_2[1:100, ], "date", c("ts1", "ts2"), group = "sh2")
  })
  
  output$am3 <- renderAmCharts({
    data("data_stock_2")
    amTimeSeries(data_stock_2[1:100, ], "date", c("ts1", "ts2"), group = "sh2")
  })
  
  output$am4 <- renderAmCharts({
    data("data_stock_2")
    amTimeSeries(data_stock_2[1:100, ], "date", c("ts1", "ts2"), group = "sh2")
  })
})
