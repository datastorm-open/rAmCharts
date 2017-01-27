output$amstock1 <- renderAmCharts({
  data("data_stock_2")
  amTimeSeries(data_stock_2, "date", c("ts1", "ts2"))
})

output$code_amstock1 <- renderText({
  "
    data('data_stock_2')
    amTimeSeries(data_stock_2, 'date', c('ts1', 'ts2'))
  "
})


output$amstock2 <- renderAmCharts({
  data('data_stock_2')
  amTimeSeries(data_stock_2, "date", c("ts1", "ts2"), bullet = "round",
               groupToPeriods = c('hh', 'DD', '10DD'),
               linewidth = c(3, 1))
})

output$code_amstock2 <- renderText({
  "
    data('data_stock_2')
    amTimeSeries(data_stock_2, 'date', c('ts1', 'ts2'), bullet = 'round',
             groupToPeriods = c('hh', 'DD', '10DD'),
             linewidth = c(3, 1))
  "
})

output$amstock3 <- renderAmCharts({
  data('data_stock_2')
  ZoomButton <- data.frame(Unit = c("DD", "DD", "MAX"), multiple = c(1, 10 ,1),
                           label = c("Day","10 days", "MAX"))
  amTimeSeries(data_stock_2, 'date', c('ts1', 'ts2'), bullet = 'round',
               ZoomButton = ZoomButton, main = 'My title',
               ylab = 'Interest', export = TRUE,
               creditsPosition = 'bottom-left', group = 'a')
  
})
output$code_amstock3 <- renderText({
  "
  data('data_stock_2')
  ZoomButton <- data.frame(Unit = c('DD', 'DD', 'MAX'), multiple = c(1, 10 ,1),
                   label = c('Day','10 days', 'MAX'))
  amTimeSeries(data_stock_2, 'date', c('ts1', 'ts2'), bullet = 'round',
              ZoomButton = ZoomButton, main = 'My title',
              ylab = 'Interest', export = TRUE,
              creditsPosition = 'bottom-left', group = 'a')
  "
})

output$amstock4 <- renderAmCharts({
  data('data_stock_2')
  amTimeSeries(data_stock_2, 'date', c('ts1', 'ts2'), aggregation = 'Sum', legend = TRUE,
               maxSeries = 1300, group = 'a')
})
output$code_amstock4 <- renderText({
  "  
  data('data_stock_2')
  amTimeSeries(data_stock_2, 'date', c('ts1', 'ts2'), aggregation = 'Sum', legend = TRUE,
              maxSeries = 1300, group = 'a')
  "
})

output$amstock5 <- renderAmCharts({
  data(data_stock_3)
  amStockMultiSet(data = data_stock_3)
})

output$code_amstock5 <- renderText({
  "  
  data(data_stock_3)
  amStockMultiSet(data = data_stock_3)
  "
})

output$amstock6 <- renderAmCharts({
  data(data_stock_3)
  amStockMultiSet(data = data_stock_3, panelColumn = c(1,2,1,1))
})

output$code_amstock6 <- renderText({
  "  
  data(data_stock_3)
  amStockMultiSet(data = data_stock_3, panelColumn = c(1,2,1,1))
  "
})

output$amstock7 <- renderAmCharts({
  data(data_stock_3)
  ZoomButton <- data.frame(Unit = c("DD", "DD", "MAX"), multiple = c(10, 15 ,1),
                           label = c("10 days","15 days", "MAX"))
  amStockMultiSet(data = data_stock_3, panelColumn = c(1,2,1,1), percentHeightPanel = c(3,1),
                  ZoomButtonPosition = "top", ZoomButton = ZoomButton, export = TRUE)
})
output$code_amstock7 <- renderText({
  "  
  data(data_stock_3)
  ZoomButton <- data.frame(Unit = c('DD', 'DD', 'MAX'), multiple = c(10, 15 ,1),
                          label = c('10 days','15 days', 'MAX'))
  amStockMultiSet(data = data_stock_3, panelColumn = c(1,2,1,1), percentHeightPanel = c(3,1),
                  ZoomButtonPosition = 'top', ZoomButton = ZoomButton, export = TRUE)
  "
})