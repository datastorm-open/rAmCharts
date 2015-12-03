output$stock1 <- renderAmCharts({
  data('data_stock1')
  pipeR::pipeline(
    amStockChart(startDuration = 0, theme = input$theme_stock),
    addDataSet(dataSet(title = 'first data set', categoryField = 'date',
                       dataProvider = data_stock1$chartData1) %>>%
                 addFieldMapping(fromField = 'value', toField = 'value') %>>%
                 addFieldMapping(fromField = 'volume', toField = 'volume')),
    addDataSet(dataSet(title = 'second data set', categoryField = 'date',
                       dataProvider = data_stock1$chartData2) %>>%
                 addFieldMapping(fromField = 'value', toField = 'value') %>>%
                 addFieldMapping(fromField = 'volume', toField = 'volume')),
    addDataSet(dataSet(title = 'third data set', categoryField = 'date',
                       dataProvider = data_stock1$chartData3) %>>%
                 addFieldMapping(fromField = 'value', toField = 'value') %>>%
                 addFieldMapping(fromField = 'volume', toField = 'volume')),
    addDataSet(dataSet(title = 'fourth data set', categoryField = 'date',
                       dataProvider = data_stock1$chartData4) %>>%
                 addFieldMapping(fromField = 'value', toField = 'value') %>>%
                 addFieldMapping(fromField = 'volume', toField = 'volume')),
    addPanel(stockPanel(showCategoryAxis = FALSE, title = 'Value', percentHeight = 70) %>>%
               addStockGraph(id = 'g1', valueField = 'value', comparable = TRUE,
                             compareField = 'value', balloonText = '[[title]] =<b>[[value]]</b>',
                             compareGraphBalloonText = '[[title]] =<b>[[value]]</b>') %>>%
               setStockLegend(periodValueTextComparing = '[[percents.value.close]]%',
                              periodValueTextRegular = '[[value.close]]') %>>%
               addListener('zoomed', 'function(event) {alert(\'zoomed\')}')),
    addPanel(stockPanel(title = 'Volume', percentHeight = 30) %>>%
               addStockGraph(valueField = 'volume', type = 'column', fillAlphas = 1) %>>%
               setStockLegend(periodValueTextRegular = '[[value.close]]')),
    setChartScrollbarSettings(graph = 'g1'),
    setChartCursorSettings(valueBalloonsEnabled = TRUE, fullWidth = TRUE,
                           cursorAlpha = 0.1, valueLineBalloonEnabled = TRUE,
                           valueLineEnabled = TRUE, valueLineAlpha = 0.5),
    setPeriodSelector(periodSelector(position = 'left') %>>%
                        addPeriod(period = 'DD', selected = TRUE, count = 7, label = '1 week') %>>%
                        addPeriod(period = 'MAX', label = 'MAX') %>>%
                        addListener('changed', 'function(event) {alert(\'changed\')}')),
    setDataSetSelector(dataSetSelector(position = 'left') %>>%
                         addListener('dataSetCompared', 'function(event) {alert(\'dataSetCompared\')}')),
    setPanelsSettings(recalculateToPercents = FALSE),
    setExport()
  )
})

output$code_stock1 <- renderText({
  "
  data('data_stock1')
  pipeR::pipeline(
    amStockChart(startDuration = 0, theme = input$theme_stock),
    addDataSet(dataSet(title = 'first data set', categoryField = 'date',
                       dataProvider = data_stock1$chartData1) %>>%
                 addFieldMapping(fromField = 'value', toField = 'value') %>>%
                 addFieldMapping(fromField = 'volume', toField = 'volume')),
    addDataSet(dataSet(title = 'second data set', categoryField = 'date',
                       dataProvider = data_stock1$chartData2) %>>%
                 addFieldMapping(fromField = 'value', toField = 'value') %>>%
                 addFieldMapping(fromField = 'volume', toField = 'volume')),
    addDataSet(dataSet(title = 'third data set', categoryField = 'date',
                       dataProvider = data_stock1$chartData3) %>>%
                 addFieldMapping(fromField = 'value', toField = 'value') %>>%
                 addFieldMapping(fromField = 'volume', toField = 'volume')),
    addDataSet(dataSet(title = 'fourth data set', categoryField = 'date',
                       dataProvider = data_stock1$chartData4) %>>%
                 addFieldMapping(fromField = 'value', toField = 'value') %>>%
                 addFieldMapping(fromField = 'volume', toField = 'volume')),
    addPanel(stockPanel(showCategoryAxis = FALSE, title = 'Value', percentHeight = 70) %>>%
               addStockGraph(id = 'g1', valueField = 'value', comparable = TRUE,
                             compareField = 'value', balloonText = '[[title]] =<b>[[value]]</b>',
                             compareGraphBalloonText = '[[title]] =<b>[[value]]</b>') %>>%
               setStockLegend(periodValueTextComparing = '[[percents.value.close]]%',
                              periodValueTextRegular = '[[value.close]]') %>>%
               addListener('zoomed', 'function(event) {alert(\'zoomed\')}')),
    addPanel(stockPanel(title = 'Volume', percentHeight = 30) %>>%
               addStockGraph(valueField = 'volume', type = 'column', fillAlphas = 1) %>>%
               setStockLegend(periodValueTextRegular = '[[value.close]]')),
    setChartScrollbarSettings(graph = 'g1'),
    setChartCursorSettings(valueBalloonsEnabled = TRUE, fullWidth = TRUE,
                           cursorAlpha = 0.1, valueLineBalloonEnabled = TRUE,
                           valueLineEnabled = TRUE, valueLineAlpha = 0.5),
    setPeriodSelector(periodSelector(position = 'left') %>>%
                        addPeriod(period = 'DD', selected = TRUE, count = 7, label = '1 week') %>>%
                        addPeriod(period = 'MAX', label = 'MAX') %>>%
                        addListener('changed', 'function(event) {alert(\'changed\')}')),
    setDataSetSelector(dataSetSelector(position = 'left') %>>%
                         addListener('dataSetCompared', 'function(event) {alert(\'dataSetCompared\')}')),
    setPanelsSettings(recalculateToPercents = FALSE)
  )
  "
})

