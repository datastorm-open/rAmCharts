#' @title Plotting multi data-sets
#' @description  amStockMultiSet compute a stock of multi data-sets, still in dev
#' 
#' @param data \code{list}, list of data.frame (same structure) first column is date, others are values
#' @param panelColumn \code{vector}, numeric vector, controle panel adding for selected series
#' 
#' @examples 
#' data(data_stock1)
#' data_stock1$chartData1$value2 <- as.numeric(data_stock1$chartData1$value) + 10
#' data_stock1$chartData2$value2 <- as.numeric(data_stock1$chartData2$value) + 10
#' data_stock1$chartData3$value2 <- as.numeric(data_stock1$chartData3$value) + 10
#' data_stock1$chartData4$value2 <- as.numeric(data_stock1$chartData4$value) + 10
#' 
#' data_stock1$chartData1$value3 <- as.numeric(data_stock1$chartData1$value) - 10
#' data_stock1$chartData2$value3 <- as.numeric(data_stock1$chartData2$value) - 10
#' data_stock1$chartData3$value3 <- as.numeric(data_stock1$chartData3$value) - 10
#' data_stock1$chartData4$value3 <- as.numeric(data_stock1$chartData4$value) - 10
#' 
#' amStockMultiSet(data = data_stock1)
#' amStockMultiSet(data = data_stock1, panelColumn = c(1,2,1,1))
#' amStockMultiSet(data = data_stock1, panelColumn = c(1,2,3,4))
#' 
#' @export
#'
amStockMultiSet <- function(data, panelColumn = NULL){
  dataset <- list()
  for(i in 1:length(data))
  {
  dataset[[i]] <-dataSet(
    title = names(data)[i], categoryField = 'date',
    dataProvider = data[[i]])
    
    fieldmap <- list()
    for(j in 2:ncol(data[[i]]))
    {
      fieldmap[[j - 1]] <- list(fromField = names(data[[i]])[j], toField = names(data[[i]])[j])
    }

    dataset[[i]]@fieldMappings <- fieldmap
  }

  if(is.null(panelColumn)){
    panelColumn <- rep(1, ncol(data[[1]]) - 1)
  }
  
  panels <- list()
  w <- 0
  z <- 0
  for(j in sort(unique(panelColumn)))
  {
  stockGraphs <- list()
  rowinclude <- which(j == panelColumn)
  k <- 0
  for(i in rowinclude)
  {
    z <- z + 1
    k <- k + 1
    stockGraphs[[k]] <- stockGraph(id = paste0("g", z), valueField = names(data[[1]])[rowinclude[k] + 1],
                                   compareField = names(data[[1]])[rowinclude[k] + 1],
                                   balloonText = paste0('[[title]] : ',
                                                        names(data[[1]])[rowinclude[k] + 1],
                                                        ' = <b>[[value]]</b>'),
                                   comparable = TRUE,
                                   compareGraphBalloonText  = paste0('[[title]] : ',
                                                              names(data[[1]])[rowinclude[k] + 1],
                                                              ' = <b>[[value]]</b>'))
    
    
  }
  w <- w + 1
  panels[[w]] <- panel(showCategoryAxis = FALSE, percentHeight = 100/length(unique(panelColumn)),
                       stockGraphs = stockGraphs)
  }
  
  pipeR::pipeline(
    amStockChart(startDuration = 0),
    setDataSets(dataset),
    setPanels(panels),
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
  
  
}
