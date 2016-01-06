#' @title Plotting wind using rAmCharts
#' 
#' @description  wind computes a windplot of the given data values.
#' 
#' 
#' @param data : columns are series of values, from week wind (first column) to strong wind (last column)
#' @param col : color of series
#' @param backTransparency : background transparency
#' @param main : title of graph
#' @param legend : add legend, TRUE or FALSE
#' @param export : add export, TRUE or FALSE
#' @param fontSize : font size
#' @param pch : symbols
#' 
#' 
#' @seealso \code{\link{amRadar}}
#' 
#' @exemple
#' require(pipeR)
#' data <- data.frame(Week = c(1, 2, 3, 4,1, 2,1, 2), Middle = c(2, 8, 1, 1,2, 8,1, 2),Strong = c(1, 1, 2, 2,1, 1 ,1, 2))
#' amWind(data, main = "", export = TRUE, col = c("#0404B4","#01DF01","#FFBF00"), backTransparency = 1 ,pch="round")
#' @import data.table
#' @rdname amWind
#' @export
amWind <- function(data, col = NULL,  backTransparency = 0.5, main = "", legend = TRUE, export = FALSE, fontSize = 15, pch = "round") {
  
  databullet <- apply( rbind(names(data),data),2,function(x){paste0("<b>", as.numeric(x[-1]),
                                                                    " </b>Observations <br><b>",round(as.numeric(x[-1])/sum(as.numeric(x[-1]))*100,2), "%</b> of wind <b>",x[1],"</b>")})
  colnames(databullet) <- paste0(  colnames(databullet),"lab")
  
  data <- data.frame(t(apply(data, 1, cumsum)))
  data$labels<-as.character(seq(0,360,length.out = nrow(data)+1)[1:nrow(data)])
  data$labels[which(data$labels=="0")] <- "N"
  data$labels[which(data$labels=="45")] <- "NE"
  data$labels[which(data$labels=="90")] <- "E"
  data$labels[which(data$labels=="135")] <- "SE"
  data$labels[which(data$labels=="180")] <- "S"
  data$labels[which(data$labels=="225")] <- "SO"
  data$labels[which(data$labels=="270")] <- "O"
  data$labels[which(data$labels=="315")] <- "NO"
  col <- rev(col) 
  datavalue <- data[, (ncol(data)-1):1, drop = FALSE]
  if(is.null(col)){col <- ""}
  oldw <- getOption("warn")
  options(warn = -1)
  constructGraph <- cbind(names(datavalue),col, backTransparency, pch)
  options(warn = oldw)
  graphs <- list()
  
  for(i in (nrow(constructGraph)):1)
  {
    graphs[[i]] <- amGraph(title = as.character(constructGraph[i,1]), lineThickness = 2, 
                           legendAlpha = 1, lineColor = "#000000", lineAlpha = 1, 
                           balloonText =  paste0("[[",as.character(constructGraph[i,1]),"lab]]"),
                           fillColors =  as.character(constructGraph[i,2]), 
                           fillAlphas = as.numeric(constructGraph[i,3]), 
                           valueField =  as.character(constructGraph[i,1]), 
                           bullet = as.character(constructGraph[i,4]),bulletAlpha = 0, legendColor = as.character(constructGraph[i,2]))
  }
  
  data <- cbind(data,databullet)
  amRadarChart(export = list(enabled = export),fontSize=fontSize) %>>% 
    setDataProvider(data) %>>% 
    setProperties(type = "radar", theme = "light", startDuration = 1, categoryField = "labels") %>>% 
    setGraphs(graphs) %>>% 
    addTitle(text = main) %>>%
    setLegend(amLegend( labelText = "[[title]]",switchable = FALSE, align="right",markerBorderColor = "#000000")) %>>% 
    addValueAxes(gridType = "circle")
}






