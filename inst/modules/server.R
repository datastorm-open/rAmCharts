
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


shinyServer(function(session, input, output) {
  
  data <- reactive({
    data_global
  })
  
  res_1 <- callModule(rAmChartsTimeSeriesServer, "ts_1", data, reactive("date"), 
                      reactive("value"), main = reactive("Series 1"),
                      color = reactive("red"), group = reactive("sh"))
  
  res_2 <- callModule(rAmChartsTimeSeriesServer, "ts_2", data, reactive("date"), 
                      reactive("value2"), main = reactive("Series 2"),
                      color = reactive("red"), group = reactive("sh"))
  

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
