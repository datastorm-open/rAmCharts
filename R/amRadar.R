#' @title Plotting radar using rAmCharts
#' 
#' @description  radar computes a radarplot of the given data values.
#' 
#' @param data first column is names "label", other columns are series of values
#' @param col color of series
#' @param backTransparency background transparency
#' @param type "polygons" or "circle", type of radar
#' @param pch symbols
#' 
#' 
#' @example examples/amRadar_examples.R
#' 
#' @import pipeR
#' @import data.table
#' @rdname amRadar
#' @export
amRadar <- function(data, col = NULL,  backTransparency = 0.5, type = "polygons", pch = "round") {
  
  #data
  data <- as.data.frame(data)
  
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
    .testLength(col, c(1, ncol(data) - 1))
  }
  
  #backTransparency
  .testNumeric(backTransparency)
  .testLength(backTransparency, c(1, ncol(data) - 1))
  sapply(backTransparency, function(X){.testInterval(X,binf = 0, bsup = 1, arg = "backTransparency")})


  #type
  .testIn(type,c("polygons", "circles"))
  
  #pch
  .testCharacter(pch)
  .testIn(pch, c( "none", "round", "square", "triangleUp", "triangleDown", "triangleLeft", 
                  "triangleRight", "bubble", "diamond", "xError", "yError"))
  .testLength(pch, c(1, ncol(data) - 1))
  
  if(is.null(col)){col <- ""}

  constructGraph <- cbind(names(datavalue),col, backTransparency, pch)

  
  graphs <- apply(constructGraph, 1, function(x) {
    amGraph(title = as.character(x[1]), balloonText = "<b>[[title]]</b><br>[[category]] : <b>[[value]]</b>", 
            lineColor =  as.character(x[2]), fillAlphas = as.numeric(x[3]), valueField =  as.character(x[1]), 
            bullet = as.character(x[4]))
  })
  
  amRadarChart() %>>% 
    setDataProvider(data) %>>% 
    setProperties(type = "radar", theme = "light", categoryField = "label") %>>% 
    setGraphs(graphs) %>>% 
    addValueAxes(gridType = type)
}