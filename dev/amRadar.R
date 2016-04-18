#' @param data : first column is names "labels", other columns are series of values
#' @param col : color of series
#' @param backTransparency : background transparency
#' @param main : title of graph
#' @param legend : add legend, TRUE or FALSE
#' @param export : add export, TRUE or FALSE
#' @param type : "polygons" or "circle", type of radar
#' @param fontSize : font size
#' @param pch : symbols
#' 
#' @exemple
#' require(pipeR)
#' data <- data.frame(labels = c("A", "Z", "E", "R", "T"), Product1 = c(1, 2, 3, 4, 2), Product2 = c(2, 8, 1, 1, 0),Product2 = c(1,1,2,2,4))
#' amRadar(data, main = "My title", export = TRUE, col = c("#0000FF","#00FF00","#FF0000"), backTransparency = c(0,0.4),type = c("polygons","round"),pch="triangleRight")
amRadar <- function(data, col = NULL,  backTransparency = 0.5, main = "", legend = TRUE, export = FALSE, type = "circle", fontSize = 15, pch = "round") {
  datavalue <- data[, 2:ncol(data), drop = FALSE]
  if(is.null(col)){col <- ""}
  oldw <- getOption("warn")
  options(warn = -1)
  constructGraph <- cbind(names(datavalue),col, backTransparency, pch)
  options(warn = oldw)
  graphs <- apply(constructGraph, 1, function(x) {
    amGraph(title = as.character(x[1]), balloonText = "<b>[[title]]</b><br>[[category]] : <b>[[value]]</b>", lineColor =  as.character(x[2]), fillAlphas = as.numeric(x[3]), valueField =  as.character(x[1]), bullet = as.character(x[4]))
  })
  
  amRadarChart(export = list(enabled = export),fontSize=fontSize) %>>% 
  setDataProvider(data) %>>% 
  setProperties(type = "radar", theme = "light", startDuration = 1, categoryField = "labels") %>>% 
  setGraphs(graphs) %>>% 
  addTitle(text = main) %>>%
  setLegend(amLegend(useGraphSettings = TRUE, labelText = "[[title]]")) %>>% 
  addValueAxes(gridType = type)
}