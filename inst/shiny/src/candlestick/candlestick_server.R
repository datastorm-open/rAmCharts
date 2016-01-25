output$candl0 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_candleStick1')
  data_candleStick1[, -1] <- apply(data_candleStick1[, -1], 2, round, 2)
  ##Plot
  pipeR::pipeline(
    amSerialChart(dataProvider = data_candleStick1, categoryField = 'category', dataDateFormat = 'YYYY-MM-DD'),
    addTitle(text = 'Candle'),
    setChartCursor(),
    setCategoryAxis(labelRotation = 45),
    addGraph(id = 'g1', openField = 'open', closeField = 'close', highField = 'high', lowField = 'low',
             valueField = 'close', fillColors = '#F0FC98', lineColor = '#CD98FC', 
             negativeFillColors = '#98FC98', egativeLineColor = '#005321',
             type = 'candlestick', fillAlphas = 0.8, 
             balloonText =  'Open:<b>[[open]]</b><br>Low:<b>[[low]]</b><br>High:<b>[[high]]</b><br>Close:<b>[[close]]</b><br>')
  )
})


output$code_candl0 <- renderText({
  "
  ##Data
  data('data_candleStick1')
  data_candleStick1[, -1] <- apply(data_candleStick1[, -1], 2, round, 2)
  ##Plot
  pipeR::pipeline(
    amSerialChart(dataProvider = data_candleStick1, categoryField = 'category', dataDateFormat = 'YYYY-MM-DD'),
    addTitle(text = 'Candle'),
    setChartCursor(),
    setCategoryAxis(labelRotation = 45),
    addGraph(id = 'g1', openField = 'open', closeField = 'close', highField = 'high', lowField = 'low',
             valueField = 'close', fillColors = '#F0FC98', lineColor = '#CD98FC', 
             negativeFillColors = '#98FC98', egativeLineColor = '#005321',
             type = 'candlestick', fillAlphas = 0.8, 
             balloonText =  'Open:<b>[[open]]</b><br>Low:<b>[[low]]</b><br>High:<b>[[high]]</b><br>Close:<b>[[close]]</b><br>')
  )
  "
})