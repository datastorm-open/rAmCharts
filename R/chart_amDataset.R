#' @title Plotting multi data-sets
#' @description  amStockMultiSet compute a stock of multi data-sets, still in dev
#' 
#' @param data \code{list}, list of data.frame (same structure) first column is date, others are values
#' @param panelColumn \code{numeric}, numeric vector, controle panel adding for selected series
#' @param ZoomButtonPosition \code{character}, zoom button position. Possible values are :
#' "left", "right", "bottom", "top"
#' @param color \code{character}, color of data-sets (in hexadecimal).
#' @param ZoomButton \code{data.frame}, 3 columns : 
#' Unit, times unit
#' multiple : multiple*unit 
#' label : button's label
#' @param precision \code{numeric}, digits precision
#' @param export \code{logical}, default set to  FALSE. TRUE to display export feature.
#' @param percentHeightPanel \code{numeric}, vector of size panel, same length than data
#' @param creditsPosition \code{character}, credits position. Possible values are :
#' "top-right", "top-left", "bottom-right", "bottom-left"
#' @param ... other first level attributes
#' 
#' @examples 
#' data(data_stock_3)
#' 
#' amStockMultiSet(data = data_stock_3)
#' amStockMultiSet(data = data_stock_3, panelColumn = c(1,2,1,1))
#' \donttest{
#' amStockMultiSet(data = data_stock_3, panelColumn = c(1,2,3,4))
#' 
#' ZoomButton <- data.frame(Unit = c("DD", "DD", "MAX"), multiple = c(1, 10 ,1),
#'                    label = c("Day","10 days", "MAX"))
#'                    ZoomButtonPosition <- "bottom"
#' amStockMultiSet(data = data_stock_3, panelColumn = c(1,2,1,1), ZoomButton = ZoomButton,
#' ZoomButtonPosition = "top")
#' 
#' amStockMultiSet(data = data_stock_3, precision = 2)
#' 
#' amStockMultiSet(data = data_stock_3, panelColumn = c(1,2,1,1), percentHeightPanel = c(3,1))
#' }
#' 
#' @export
amStockMultiSet <- function(data,
                            panelColumn = NULL,
                            ZoomButtonPosition = "bottom",
                            ZoomButton = data.frame(Unit = "MAX",
                                                    multiple = 1,
                                                    label ="All"),
                            color = c("#2E2EFE", "#31B404", "#FF4000"),
                            precision = 1,
                            export = FALSE,
                            percentHeightPanel = NULL,
                            creditsPosition = "top-right",
                            ...){

  
  
  #creditsPosition
  .testCharacterLength1(creditsPosition)
  .testIn(creditsPosition, c("top-right", "top-left", "bottom-right", "bottom-left"))
  
  
  .testNumericLength1(precision)
  .testCharacterLength1(ZoomButtonPosition)
  
  #color
  .testCharacter(char = color)
  
  #Export
  .testLogicalLength1(logi = export)
  
  
  if(is.null(panelColumn)){
    panelColumn <- rep(1, ncol(data[[1]]) - 1)
  }
  .testNumeric(panelColumn)
  
  if(is.null(percentHeightPanel)){
    percentHeightPanel <- rep(100/length(unique(panelColumn)), length(unique(panelColumn)))
  }else{
    .testNumeric(percentHeightPanel)
    percentHeightPanel <-percentHeightPanel/sum(percentHeightPanel) * 100
    
  }

  
  
  
  
  
  
  
  
  dataset <- list()
  for(i in 1:length(data))
  {
    
  dataset[[i]] <-dataSet(
    title = names(data)[i], categoryField = 'date',
    
    dataProvider = data[[i]], color = color[(i - 1)%%(length(data) - 1) + 1])
    
    fieldmap <- list()
    for(j in 2:ncol(data[[i]]))
    {
      fieldmap[[j - 1]] <- list(fromField = names(data[[i]])[j], toField = names(data[[i]])[j])
    }

    dataset[[i]]@fieldMappings <- fieldmap
  }


  
  
  
  periodZoom <- periodSelector( position = ZoomButtonPosition ,inputFieldsEnabled = FALSE)
  
  if (!is.null(ZoomButton)) {
    for (i in 1:nrow(ZoomButton)) {
      if (i == 1) {
        periodZoom <- pipeR::pipeline(periodZoom,
                                      addPeriod(period = ZoomButton$Unit[i],
                                                selected = TRUE, count = ZoomButton$multiple[i],
                                                label =  ZoomButton$label[i])
        )
      } else {
        periodZoom <- pipeR::pipeline(periodZoom,
                                      addPeriod(period = ZoomButton$Unit[i],
                                                count = ZoomButton$multiple[i],
                                                label =  ZoomButton$label[i])
        )
      }
    }
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
  panels[[w]] <- panel(showCategoryAxis = FALSE, percentHeight = percentHeightPanel[w],
                       stockGraphs = stockGraphs,
                       precision = precision)
  }
  pipeR::pipeline(
    amStockChart(startDuration = 0),
    setDataSets(dataset),
    setPanels(panels),
    setChartCursorSettings(valueBalloonsEnabled = TRUE, fullWidth = TRUE,
                           cursorAlpha = 0.1, valueLineBalloonEnabled = TRUE,
                           valueLineEnabled = TRUE, valueLineAlpha = 0.5),
    setPeriodSelector(periodZoom),
    setDataSetSelector(position = 'left'),
    setPanelsSettings(recalculateToPercents = FALSE ,creditsPosition = creditsPosition),
    setExport(enabled = export)
  )
}
