output$stock1 <- renderAmCharts({
  data('data_stock1')
  pipeR::pipeline(
    amStockChart(startDuration = 0),
    addDataSet(pipeR::pipeline(dataSet(title = 'first data set', categoryField = 'date',
                                       dataProvider = data_stock1$chartData1),
                               addFieldMapping(fromField = 'value', toField = 'value'),
                               addFieldMapping(fromField = 'volume', toField = 'volume'))),
    addDataSet(pipeR::pipeline(dataSet(title = 'second data set', categoryField = 'date',
                                       dataProvider = data_stock1$chartData2),
                               addFieldMapping(fromField = 'value', toField = 'value'),
                               addFieldMapping(fromField = 'volume', toField = 'volume'))),
    addDataSet(pipeR::pipeline(dataSet(title = 'third data set', categoryField = 'date',
                                       dataProvider = data_stock1$chartData3),
                               addFieldMapping(fromField = 'value', toField = 'value'),
                               addFieldMapping(fromField = 'volume', toField = 'volume'))),
    addDataSet(pipeR::pipeline(dataSet(title = 'fourth data set', categoryField = 'date',
                                       dataProvider = data_stock1$chartData4),
                               addFieldMapping(fromField = 'value', toField = 'value'),
                               addFieldMapping(fromField = 'volume', toField = 'volume'))),
    addPanel(pipeR::pipeline(stockPanel(showCategoryAxis = FALSE, title = 'Value', percentHeight = 70),
                             addStockGraph(id = 'g1', valueField = 'value', comparable = TRUE,
                                           compareField = 'value', balloonText = '[[title]] =<b>[[value]]</b>',
                                           compareGraphBalloonText = '[[title]] =<b>[[value]]</b>'),
                             setStockLegend(periodValueTextComparing = '[[percents.value.close]]%',
                                            periodValueTextRegular = '[[value.close]]'))),
    addPanel(pipeR::pipeline(stockPanel(title = 'Volume', percentHeight = 30),
                             addStockGraph(valueField = 'volume', type = 'column', fillAlphas = 1),
                             setStockLegend(periodValueTextRegular = '[[value.close]]'))),
    setChartScrollbarSettings(graph = 'g1'),
    setChartCursorSettings(valueBalloonsEnabled = TRUE, fullWidth = TRUE,
                           cursorAlpha = 0.1, valueLineBalloonEnabled = TRUE,
                           valueLineEnabled = TRUE, valueLineAlpha = 0.5),
    setPeriodSelector(pipeR::pipeline(periodSelector(position = 'left'),
                                      addPeriod(period = 'DD', selected = TRUE, count = 7, label = '1 week'),
                                      addPeriod(period = 'MAX', label = 'MAX'))),
    setDataSetSelector(position = 'left'),
    setPanelsSettings(recalculateToPercents = FALSE)
  )
})

output$code_stock1 <- renderText({
  "
  data('data_stock1')
  pipeR::pipeline(
    amStockChart(startDuration = 0),
    setExport(),
    addDataSet(
      pipeR::pipeline(
        dataSet(title = 'first data set', categoryField = 'date',
                dataProvider = data_stock1$chartData1),
        addFieldMapping(fromField = 'value', toField = 'value'),
        addFieldMapping(fromField = 'volume', toField = 'volume'))
    ),
    addDataSet(
      pipeR::pipeline(dataSet(title = 'second data set', categoryField = 'date',
                              dataProvider = data_stock1$chartData2),
                      addFieldMapping(fromField = 'value', toField = 'value'),
                      addFieldMapping(fromField = 'volume', toField = 'volume'))
    ),
    addDataSet(
      pipeR::pipeline(dataSet(title = 'third data set', categoryField = 'date',
                              dataProvider = data_stock1$chartData3),
                      addFieldMapping(fromField = 'value', toField = 'value'),
                      addFieldMapping(fromField = 'volume', toField = 'volume'))
    ),
    addDataSet(
      pipeR::pipeline(dataSet(title = 'fourth data set', categoryField = 'date',
                              dataProvider = data_stock1$chartData4),
                      addFieldMapping(fromField = 'value', toField = 'value'),
                      addFieldMapping(fromField = 'volume', toField = 'volume'))
    ),
    addPanel(
      pipeR::pipeline(
        stockPanel(showCategoryAxis = FALSE, title = 'Value', percentHeight = 70),
        addStockGraph(id = 'g1', valueField = 'value', comparable = TRUE,
                      compareField = 'value', balloonText = '[[title]] =<b>[[value]]</b>',
                      compareGraphBalloonText = '[[title]] =<b>[[value]]</b>'),
        setStockLegend(periodValueTextComparing = '[[percents.value.close]]%',
                       periodValueTextRegular = '[[value.close]]'))
    ),
    addPanel(
      pipeR::pipeline(stockPanel(title = 'Volume', percentHeight = 30),
                      addStockGraph(valueField = 'volume', type = 'column', fillAlphas = 1),
                      setStockLegend(periodValueTextRegular = '[[value.close]]'))
    ),
    setChartScrollbarSettings(graph = 'g1'),
    setChartCursorSettings(valueBalloonsEnabled = TRUE, fullWidth = TRUE,
                           cursorAlpha = 0.1, valueLineBalloonEnabled = TRUE,
                           valueLineEnabled = TRUE, valueLineAlpha = 0.5),
    setPeriodSelector(
      pipeR::pipeline(periodSelector(position = 'left'),
                      addPeriod(period = 'DD', selected = TRUE, count = 7, label = '1 week'),
                      addPeriod(period = 'MAX', label = 'MAX'))
    ),
    setDataSetSelector(position = 'left'),
    setPanelsSettings(recalculateToPercents = FALSE)
  )
  "
})






output$stock2 <- renderAmCharts({
  ## Data
  times <- as.POSIXct(seq(-60 * 60 * 24 * 50 + 1, 0, by = 3600), origin = Sys.time(), tz = 'UTC')
  times <- round(times,'hours')
  times <- data.frame(times)
  times$Mesure <- 1:length(times$times) + rep(cos(seq(-pi,pi,length.out = 100)), 12) * 500 + runif(length(times$times)) * 200
  
  mycategoryBalloonDateFormat <- list(list(period = 'YYYY', format = 'YYYY'),
                                      list(period='MM', format = 'YYYY-MM'), 
                                      list(period = 'WW', format = 'YYYY-MM-DD'),
                                      list(period='DD', format = 'YYYY-MM-DD'), 
                                      list(period = 'hh', format = 'YYYY-MM-DD JJ:NN'),
                                      list(period='mm', format = 'YYYY-MM-DD JJ:NN'), 
                                      list(period = 'ss', format = 'YYYY-MM-DD JJ:NN:ss'),
                                      list(period='fff', format = 'YYYY-MM-DD JJ:NN:ss'))
  ## Plot
  pipeR::pipeline(
    amStockChart(dataDateFormat = 'YYYY-MM-DD JJ:NN:ss') ,
    addDataSet(pipeR::pipeline(
      dataSet(title = 'first data set', categoryField = 'times') ,
      setDataProvider(times,keepNA=FALSE),
      addFieldMapping(fromField = 'Mesure', toField = 'Mesure'))),
    addPanel(pipeR::pipeline(
      stockPanel(showCategoryAxis = TRUE, title = 'Value') ,
      addStockGraph(id = 'g1',connect=FALSE, valueField = 'Mesure', comparable = TRUE,periodValue = 'Average',
                    compareField = 'Mesure', balloonText = 'Value : <b>[[value]] Unit</b>', precision = 0,
                    compareGraphBalloonText = '[[title]] =<b>[[value]]</b>'))),
    setChartScrollbarSettings(graph = 'g1'),
    setChartCursorSettings( valueBalloonsEnabled = TRUE, fullWidth = TRUE,
                            cursorAlpha = 0.1, valueLineBalloonEnabled = TRUE,
                            valueLineEnabled = TRUE, valueLineAlpha = 0.5,
                            categoryBalloonDateFormats = mycategoryBalloonDateFormat),
    setPeriodSelector(pipeR::pipeline( periodSelector( position = 'bottom' ,inputFieldsEnabled=FALSE) ,
                                       addPeriod( period = 'DD', selected = TRUE, count = 1, label = '1 day') ,
                                       addPeriod( period = 'WW', count = 1, label = '1 week' ) ,
                                       addPeriod( period = 'MAX', label = 'All' ))),
    setCategoryAxesSettings(parseDates = TRUE, minPeriod = '1hh',
                            groupToPeriods = c('hh','3hh', '12hh','1DD'),maxSeries = 50),
    setPanelsSettings(recalculateToPercents = 'never', creditsPosition='top-left')
  )
})



output$code_stock2 <- renderText({
  "
  ## Data
  times <- as.POSIXct(seq(-60 * 60 * 24 * 50 + 1, 0, by = 3600), origin = Sys.time(), tz = 'UTC')
  times <- round(times,'hours')
  times <- data.frame(times)
  times$Mesure <- 1:length(times$times) + rep(cos(seq(-pi,pi,length.out = 100)), 12) * 500 + runif(length(times$times)) * 200
  
  mycategoryBalloonDateFormat <- list(list(period = 'YYYY', format = 'YYYY'),
                                      list(period='MM', format = 'YYYY-MM'), 
                                      list(period = 'WW', format = 'YYYY-MM-DD'),
                                      list(period='DD', format = 'YYYY-MM-DD'), 
                                      list(period = 'hh', format = 'YYYY-MM-DD JJ:NN'),
                                      list(period='mm', format = 'YYYY-MM-DD JJ:NN'), 
                                      list(period = 'ss', format = 'YYYY-MM-DD JJ:NN:ss'),
                                      list(period='fff', format = 'YYYY-MM-DD JJ:NN:ss'))
  ## Plot
  pipeR::pipeline(
    amStockChart(dataDateFormat = 'YYYY-MM-DD JJ:NN:ss') ,
    addDataSet(pipeR::pipeline(
      dataSet(title = 'first data set', categoryField = 'times') ,
      setDataProvider(times,keepNA=FALSE),
      addFieldMapping(fromField = 'Mesure', toField = 'Mesure'))),
    addPanel(pipeR::pipeline(
      stockPanel(showCategoryAxis = TRUE, title = 'Value') ,
      addStockGraph(id = 'g1',connect=FALSE, valueField = 'Mesure', comparable = TRUE,periodValue = 'Average',
                    compareField = 'Mesure', balloonText = 'Value : <b>[[value]] Unit</b>', precision = 0,
                    compareGraphBalloonText = '[[title]] =<b>[[value]]</b>'))),
    setChartScrollbarSettings(graph = 'g1'),
    setChartCursorSettings( valueBalloonsEnabled = TRUE, fullWidth = TRUE,
                            cursorAlpha = 0.1, valueLineBalloonEnabled = TRUE,
                            valueLineEnabled = TRUE, valueLineAlpha = 0.5,
                            categoryBalloonDateFormats = mycategoryBalloonDateFormat),
    setPeriodSelector(pipeR::pipeline( periodSelector( position = 'bottom' ,inputFieldsEnabled=FALSE) ,
                                       addPeriod( period = 'DD', selected = TRUE, count = 1, label = '1 day') ,
                                       addPeriod( period = 'WW', count = 1, label = '1 week' ) ,
                                       addPeriod( period = 'MAX', label = 'All' ))),
    setCategoryAxesSettings(parseDates = TRUE, minPeriod = '1hh',
                            groupToPeriods = c('hh','3hh', '12hh','1DD'),maxSeries = 50),
    setPanelsSettings(recalculateToPercents = 'never', creditsPosition='top-left')
  )
  "
})
