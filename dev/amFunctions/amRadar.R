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
#' @examples
#' data <- data.frame(label = c("A", "Z", "E", "R", "T"),
#'                    Product1 = c(1, 2, 3, 4, 2), 
#'                    Product2 = c(2, 8, 1, 1, 0),
#'                    Product3 = c(1,1,2,2,4))
#' amRadar(data)
#' 
#' amRadar(data, col = c("#0000FF", "#00FF00", "#FF0000"), 
#'         backTransparency = c(0, 0.4, 0.6),
#'         type = c("polygons"),
#'         pch = "triangleRight")
#' 
#' amRadar(data,backTransparency = c(0,0.4,0.6),
#'         type = c("circles"), pch = "triangleLeft" )%>>% amOptions(legend = TRUE)
#' 
#' 
#' amRadar(data,backTransparency = c(0,0.4,0.6),
#'         type = c("circles"), pch = "triangleLeft" ) %>>% 
#'   amOptions(legend = TRUE, legendPosision = "bottom", export = TRUE, main = "My radar")
#' 
#' @import pipeR
#' @import data.table
#' @rdname amRadar
#' @export
amRadar <- function(data, col = NULL,  backTransparency = 0.5, type = "polygons", pch = "round") {
  
  #data
  data <- as.data.frame(data)
  if(!"label"%in%colnames(data)){
    stop ("One column of data must be named 'label'")
  }

  datavalue <- data[, -which(colnames(data)=="label"), drop = FALSE]
  invisible(sapply(names(datavalue),function(X){
    .testNumeric(datavalue[,X], arg = X)
  }))
  
  #Test
  #col
  .testCharacter(data$label)
  if(!is.null(col))
  {
    .testCharacter(col)
    .testLength(col, c(1, ncol(data) - 1))
  }
  
  #backTransparency
  .testNumeric(backTransparency)
  .testLength(backTransparency, c(1, ncol(data) - 1))
  sapply(backTransparency, function(X){.testInterval(X, arg = "backTransparency")})


  #type
  .testIn(type,c("polygons", "circles"))
  
  #pch
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