#' @title Plotting radar using rAmCharts
#' @description  radar computes a radarplot of the given data values.
#' 
#' @param data a data frame first column is named "label" (character), other columns are series of values,
#' see \link{data_radar}.
#' @param col  \code{character} color(s) of serie(s) hexadecimal like "#00FF00".
#' @param backTransparency \code{numeric} background transparency, between 0 and 1.
#' @param type \code{character} "polygons" or "circle", type of radar.
#' @param pch \code{character} symbols must be in "round", "square", "triangleUp", 
#' "triangleDown", "triangleLeft", "triangleRight", "bubble", 
#' "diamond", "xError", "yError". 
#' @param ... see \link{amOptions} for more options.
#' 
#' @example examples/amRadar_examples.R
#' 
#' @import pipeR
#' @import data.table
#' @rdname amRadar
#' @export
#' 
amRadar <- function(data, col = NULL,  backTransparency = 0.5, type = "polygons", pch = "round", ...)
{
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
  
  if(is.null(col)){col <- ""}
  
  constructGraph <- cbind(names(datavalue),col, backTransparency, pch)
  
  
  graphs <- apply(constructGraph, 1, function(x) {
    amGraph(title = as.character(x[1]), balloonText = "<b>[[title]]</b><br>[[category]] : <b>[[value]]</b>", 
            lineColor =  as.character(x[2]), fillAlphas = as.numeric(x[3]), valueField =  as.character(x[1]), 
            bullet = as.character(x[4]))
  })
  
  res <- amRadarChart(dataProvider = data, categoryField = "label") %>>% 
    setGraphs(graphs) %>>% 
    addValueAxes(gridType = type)
  
  res <- amOptions(res, ...)
  res
  
}