output$cds0 <- rAmCharts::renderAmCharts({
  # load data
  data('data_candleStick2')
  
  # draw chart
  amCandlestick(data = data_candleStick2)
})



output$code_cds0 <- renderText({
  "
  # load data
  data('data_candleStick2')

  # draw chart
  amCandlestick(data = data_candleStick2)
  "
})


output$cds1 <- rAmCharts::renderAmCharts({
  # load data
  data('data_candleStick2')
  
  # draw chart
  amCandlestick(data = data_candleStick2, scrollbar = TRUE, horiz = TRUE)
})



output$code_cds1 <- renderText({
  "
  # load data
  data('data_candleStick2')
  
  # draw chart
  amCandlestick(data = data_candleStick2, scrollbar = TRUE, horiz = TRUE)
  "
})




output$cds2 <- rAmCharts::renderAmCharts({
  # load data
  data('data_candleStick2')
  
  # draw chart
  amCandlestick(data = data_candleStick2, dataDateFormat = 'YYYY-MM-DD', period_type = 'month')
})

output$code_cds2 <- renderText({
  "
  # load data
  data('data_candleStick2')
  
  # draw chart
  amCandlestick(data = data_candleStick2, dataDateFormat = 'YYYY-MM-DD', period_type = 'month')
  "
})