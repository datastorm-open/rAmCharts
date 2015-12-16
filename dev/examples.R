# BASIC EXAMPLES ---
library(pipeR)
library(data.table)

data('data_stock1')
guides_obj <- lapply(seq(1, 20, by = 2), function (day) {
  guide(date = paste0('10/', day, '/2015'), toDate = paste0('10/', day+1, '/2015'),
        lineColor = '#CC0000', lineAlpha = 1, fillAlpha = 02,
        fillColor = '#CC0000', dashLength = 2, inside = TRUE,
        labelRotation = 90, label = paste("Day", day))
})
pipeR::pipeline(
  amStockChart(startDuration = 0),
  addDataSet(dataSet(title = 'first data set', categoryField = 'date',
                     dataProvider = data_stock1$chartData1) %>>%
               addFieldMapping(fromField = 'value', toField = 'value') %>>%
               addFieldMapping(fromField = 'volume', toField = 'volume')),
  addDataSet(dataSet(title = 'fourth data set', categoryField = 'date',
                     dataProvider = data_stock1$chartData4) %>>%
               addFieldMapping(fromField = 'value', toField = 'value') %>>%
               addFieldMapping(fromField = 'volume', toField = 'volume')),
  addPanel(stockPanel(showCategoryAxis = FALSE, title = 'Value', percentHeight = 70) %>>%
             addStockGraph(id = 'g1', valueField = 'value', comparable = TRUE,
                           compareField = 'value') %>>%
             setGuides(guides_obj)),
  addPanel(stockPanel(title = 'Volume', percentHeight = 30) %>>%
             addStockGraph(valueField = 'volume', type = 'column', fillAlphas = 1)),
  setDataSetSelector(position = 'left'),
  setPanelsSettings(recalculateToPercents = FALSE)
)