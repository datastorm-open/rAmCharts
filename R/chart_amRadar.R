#' @title Plotting radar
#' 
#' @description  radar computes a radarplot of the given data values.
#' 
#' @param data  \code{data.frame} first column is named "label" (character), other columns are series of values,
#' see \link{data_radar}.
#' @param col  \code{character}, color(s) of serie(s) hexadecimal like "#00FF00".
#' @param backTransparency \code{numeric}, background transparency, between 0 and 1.
#' @param type \code{character}, type of radar. Possible values are : "polygons" or "circle".
#' @param pch \code{character}, points symbols. Possible values are : "round", "square", "triangleUp", 
#' "triangleDown", "triangleLeft", "triangleRight", "bubble", 
#' "diamond", "xError", "yError". 
#' @param xlim \code{numeric}, x range.
#' @param ... see \code{\link{amOptions}} for more options.
#' 
#' @examples
#' data("data_radar")
#' amRadar(data_radar)
#' 
#' \dontrun{
#' 
#' # Other examples available which can be time consuming depending on your configuration.
#' 
#' if (requireNamespace("pipeR", quietly = TRUE)) {
#' require(pipeR)
#' 
#' # Change color
#' amRadar(data_radar, col = "#FF0000")
#' amRadar(data_radar, col = c("#0000FF", "#00FF00", "#FF0000"))
#' 
#' 
#' # Change backTransparency
#' amRadar(data_radar, backTransparency = 0.6)
#' amRadar(data_radar, backTransparency = c(0, 0.4, 0.6))
#' 
#' 
#' # Change type
#' amRadar(data_radar, type = "circles")
#' 
#' # Change pch
#' amRadar(data_radar,  pch = "triangleRight")
#' amRadar(data_radar,  pch = "triangleLeft")
#' 
#' # Min-Max
#' amRadar(data_radar, xlim = c(0, 8))
#' }
#' }
#' 
#' 
#' @import pipeR
#' @import data.table
#' 
#' @rdname amRadar
#' 
#' @seealso \link{amOptions}, \link{amBarplot}, \link{amBoxplot}, \link{amHist}, \link{amPie},
#' \link{amPlot}, \link{amTimeSeries}, \link{amStockMultiSet}, \link{amBullet}, \link{amRadar}, 
#' \link{amWind}, \link{amFunnel}, \link{amAngularGauge}, \link{amSolidGauge}, \link{amMekko},
#' \link{amCandlestick}, \link{amFloatingBar}, \link{amOHLC}, \link{amWaterfall}
#' 
#' @export
#'
#' @references See online documentation \url{https://datastorm-open.github.io/introduction_ramcharts/}
#' and \link{amChartsAPI}
#' 
amRadar <- function(data, col = NULL,  backTransparency = 0.5, type = "polygons", pch = "round",
                    xlim = NULL, ...) {
  #data
  data <- .testFormatData(data)
  
  .testIn("label", colnames(data))
  

  #Test Numeric data
  datavalue <- data[, -which(colnames(data)=="label"), drop = FALSE]
  invisible(sapply(names(datavalue),function(X){
    .testNumeric(datavalue[,X], arg = X)
  }))
  
  
  
  
  #Test
  .testCharacter(data$label)
  
  #col
  if(!is.null(col))
  {
    .testCharacter(col)
    .testLength(param = col, len = c(1, ncol(data) - 1))
  }
  
  #backTransparency
  .testNumeric(num = backTransparency)
  .testLength(param = backTransparency, len = c(1, ncol(data) - 1))
  sapply(backTransparency, function(X){.testInterval(X,binf = 0, bsup = 1, arg = "backTransparency")})
  
  
  #type
  .testIn(vect = type, control = c("polygons", "circles"))
  
  #pch
  .testCharacter(char = pch)
  .testIn(vect = pch, control = c( "none", "round", "square", "triangleUp", "triangleDown", "triangleLeft", 
                                   "triangleRight", "bubble", "diamond", "xError", "yError"))
  .testLength(param = pch, len = c(1, ncol(data) - 1))
  
  if(!is.null(xlim))
  {
  .testNumeric(num = xlim)
  }
  
  if(is.null(col)){col <- ""}
  
  constructGraph <- cbind(names(datavalue),col, backTransparency, pch)
  
  
  graphs <- apply(constructGraph, 1, function(x) {
    amGraph(title = as.character(x[1]), balloonText = "<b>[[title]]</b><br>[[category]] : <b>[[value]]</b>", 
            lineColor =  as.character(x[2]), fillAlphas = as.numeric(x[3]), valueField =  as.character(x[1]), 
            bullet = as.character(x[4]))
  })
  
  if(is.null(xlim))
  {
  valueaxe <- valueAxis(gridType = type)
  }else{
    if(length(xlim) == 1){
      valueaxe <- valueAxis(gridType = type, minimum = xlim)
    }
    if(length(xlim) == 2){
      valueaxe <- valueAxis(gridType = type, minimum = xlim[1], maximum = xlim[2])
    }
  }

  res <- amRadarChart(dataProvider = data, categoryField = "label") %>>% 
    setGraphs(graphs) %>>% 
    addValueAxis(valueaxe)
  

  res <- amOptions(res, ...)
  res
  
}