#' @title Plotting radar using rAmCharts
#' 
#' @description  radar computes a radarplot of the given data values.
#' 
#' @param data : first column is names "label", other columns are series of values
#' @param col : color of series
#' @param backTransparency : background transparency
#' @param main : title of graph
#' @param legend : add legend, TRUE or FALSE
#' @param export : add export, TRUE or FALSE
#' @param type : "polygons" or "circle", type of radar
#' @param mainSize : font size
#' @param pch : symbols
#' 
#' 
#' @examples
#' data <- data.frame(label = c("A", "Z", "E", "R", "T"),
#'                    Product1 = c(1, 2, 3, 4, 2), 
#'                    Product2 = c(2, 8, 1, 1, 0),
#'                    Product3 = c(1,1,2,2,4))
#' amRadar(data)
#' amRadar(data, main = "My title", export = TRUE,
#'         col = c("#0000FF","#00FF00","#FF0000"), 
#'         backTransparency = c(0,0.4),
#'         type = c("polygons"),
#'         pch = "triangleRight")
#' 
#' @import pipeR
#' @import data.table
#' @rdname amRadar
#' @export
amRadar <- function(data, col = NULL,  backTransparency = 0.5, main = "", legend = TRUE, export = FALSE, 
                    type = "polygons", mainSize = 15, pch = "round") {
  
  if(!is.data.frame(data)){
    stop ("data must be a data frame")
  }
  
  if(!"label"%in%colnames(data)){
    stop ("One column of data must be named 'label'")
  }
  
  if(!class(data$label)%in%c("factor","character"))
  {
    stop ("Class of data$label must be character or factor")
  }
  
  if(!is.null(col))
  {
    if(!class(col)%in%c("factor","character"))
    {
      stop ("Class of col must be character or factor")
    }
  }
  
  for(i in 1:length(backTransparency))
  {
    if(!(class(backTransparency[i])%in%c("numeric")  | backTransparency[i]<0  |  backTransparency[i]>1))
    {
      stop ("backTransparency must be numeric between 0 and 1")
    }
  }
  
  if(!class(main)%in%c("character"))
  {
    stop ("main must be character")
  }
  
  if(!legend%in%c(TRUE,FALSE))
  {
    stop ("legend must be TRUE or FALSE")
  }
  
  if(!export%in%c(TRUE,FALSE))
  {
    stop ("export must be TRUE or FALSE")
  }
  
  for(i in 1:length(type))
  {
    if(!type[i]%in%c("polygons", "circles"))
    {
      stop ("type must be polygons or circles")
    }
  }
  
  if(!class(mainSize)%in% c("numeric"))
  {
    stop ("mainSize must be a numeric")
  }
  
  for(i in 1:length(pch))
  {
    if(!pch[i]%in% c( "none", "round", "square", "triangleUp", "triangleDown", "triangleLeft", 
                      "triangleRight", "bubble", "diamond", "xError", "yError"))
    {
      stop ("pch must be in : none, round, square, triangleUp, triangleDown, triangleLeft, 
            triangleRight, bubble, diamond, xError, yError")
    }
  }
  
  datavalue <- data[, -which(colnames(data)=="label"), drop = FALSE]
  
  for(i in 1:ncol(datavalue))
  {
    if(!class(datavalue[,i]) == "numeric")
    {
      stop ("All column of data except label must be numeric")
    }
  }
  
  if(is.null(col)){col <- ""}
  oldw <- getOption("warn")
  options(warn = -1)
  constructGraph <- cbind(names(datavalue),col, backTransparency, pch)
  options(warn = oldw)
  graphs <- apply(constructGraph, 1, function(x) {
    amGraph(title = as.character(x[1]), balloonText = "<b>[[title]]</b><br>[[category]] : <b>[[value]]</b>", 
            lineColor =  as.character(x[2]), fillAlphas = as.numeric(x[3]), valueField =  as.character(x[1]), 
            bullet = as.character(x[4]))
  })
  
  amRadarChart(export = list(enabled = export),fontSize=mainSize) %>>% 
    setDataProvider(data) %>>% 
    setProperties(type = "radar", theme = "light", categoryField = "label") %>>% 
    setGraphs(graphs) %>>% 
    addTitle(text = main) %>>%
    setLegend(amLegend(useGraphSettings = TRUE, labelText = "[[title]]")) %>>% 
    addValueAxes(gridType = type)
}