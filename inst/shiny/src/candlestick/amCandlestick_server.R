output$cds0 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_candleStick2')
  ##Plot
  amCandlestick(data = data_candleStick2)
})



output$code_cds0 <- renderText({
  "
  ##Data
  data('data_candleStick2')
  ##Plot
  amCandlestick(data = data_candleStick2)
  "
})


output$cds1 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_candleStick2')
  ##Plot
  amCandlestick(data = data_candleStick2, scrollbar = TRUE, horiz = TRUE)
})



output$code_cds1 <- renderText({
  "
  ##Data
  data('data_candleStick2')
  ##Plot
  amCandlestick(data = data_candleStick2, scrollbar = TRUE, horiz = TRUE)
  "
})




output$cds2 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_candleStick2')
  ##Plot
  amCandlestick(data = data_candleStick2, dataDateFormat = 'YYYY-MM-DD', period_type = 'month')
})

output$code_cds2 <- renderText({
  "
  ##Data
  data('data_candleStick2')
  ##Plot
  amCandlestick(data = data_candleStick2, dataDateFormat = 'YYYY-MM-DD', period_type = 'month')
  "
})