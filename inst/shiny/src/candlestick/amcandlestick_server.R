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
  data_candleStick2$category <- c('2015-01-01', '2015-02-01', '2015-03-01',
                                  '2015-04-01', '2015-05-01', '2015-06-01',
                                  '2015-07-01', '2015-08-01', '2015-09-01',
                                  '2015-10-01', '2015-11-01', '2015-12-01')
  ##Plot
  amCandlestick(data = data_candleStick2, dataDateFormat = 'YYYY-MM-DD', minPeriod = 'MM')
})

output$code_cds2 <- renderText({
  "
  ##Data
  data('data_candleStick2')
  data_candleStick2$category <- c('2015-01-01', '2015-02-01', '2015-03-01',
                                  '2015-04-01', '2015-05-01', '2015-06-01',
                                  '2015-07-01', '2015-08-01', '2015-09-01',
                                  '2015-10-01', '2015-11-01', '2015-12-01')
  ##Plot
  amCandlestick(data = data_candleStick2, dataDateFormat = 'YYYY-MM-DD', minPeriod = 'MM')
  "
})