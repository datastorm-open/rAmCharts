#' @title Plotting wind
#' 
#' @description  amWind computes a windplot of the given data values.
#' 
#' @param data \code{data.frame}, a dataframe which columns are series of values, 
#' from weakest wind (first column) to stronger wind (last column). See \link{data_wind}.
#' @param col  \code{character}, color(s) of serie(s) hexadecimal like "#00FF00".
#' @param backTransparency \code{numeric}, background transparency, between 0 and 1.
#' @param ... see \code{\link{amOptions}} for more options.
#' 
#' @examples
#' 
#' \dontrun{
#' data("data_wind")
#' amWind(data_wind)
#' 
#' 
#' # Other examples available which can be time consuming depending on your configuration.
#' 
#' # Change color
#' amWind(data = data_wind, col = "#0404B4")
#' amWind(data = data_wind, col = c("#0404B4","#01DF01","#FFBF00"))
#' 
#' # Change backTransparency
#' amWind(data = data_wind, col = c("#0404B4","#01DF01","#FFBF00"), backTransparency = 0.1)
#' amWind(data = data_wind, col = c("#0404B4","#01DF01","#FFBF00"), backTransparency = 1)
#' amWind(data = data_wind, col = c("#0404B4","#01DF01","#FFBF00"), backTransparency = c(0.1, 0.1, 1))
#' 
#' }
#'
#' @import data.table
#' @import pipeR
#' 
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
amWind <- function(data, col = NULL,  backTransparency = 0.5, ...) {
  
  
  
  #data
  data <- .testFormatData(data)
  
  
  # backTransparency
  .testNumeric(num = backTransparency)
  .testLength(param = backTransparency, len = c(1, ncol(data)))
  sapply(backTransparency, function(X){.testInterval(num = X,binf = 0, bsup = 1, arg = "backTransparency")})
  
  # col
  if(!is.null(col)) {
    .testCharacter(char = col)
    .testLength(param = col, len = c(1, ncol(data)))
  }
  
  
  # Test data numeric
  dt <- names(data)
  apply(data, 2, function(X){
    .testNumeric(num = X)})
  data1 <- data
  databullet <- apply( rbind(names(data), data), 2, function(x){paste0("<b>", as.numeric(x[-1]),
                    " </b>Observations <br><b>", round(as.numeric(x[-1])/sum(as.numeric(x[-1]))*100, 2),
                    "%</b> of wind <b>", x[1], "</b>")})
  
  

  
  colnames(databullet) <- paste0(  colnames(databullet),"lab")
  data <- round(data * 100 / sum(data), 2)
  data <- data.frame(t(apply(data, 1, cumsum)))
  data$labels<-as.character(seq(0,360,length.out = nrow(data)+1)[1:nrow(data)])
  data$labels[which(data$labels=="0")] <- "N"
  data$labels[which(data$labels=="45")] <- "NE"
  data$labels[which(data$labels=="90")] <- "E"
  data$labels[which(data$labels=="135")] <- "SE"
  data$labels[which(data$labels=="180")] <- "S"
  data$labels[which(data$labels=="225")] <- "SW"
  data$labels[which(data$labels=="270")] <- "W"
  data$labels[which(data$labels=="315")] <- "NW"
  

  
  
  lib2 <- round(data1 / rowSums(data1) * 100, 2)
  
  for(i in 1:ncol(lib2)){
    databullet[, i] <- paste0(databullet[, i], "<br><b>", lib2[,i ],"%</b> of wind <b>", data$labels, "</b>")
  }
  
 
  
  col <- rev(col) 
  datavalue <- data[, (ncol(data)-1) : 1, drop = FALSE]
  if(is.null(col)){col <- ""}
  oldw <- getOption("warn")
  options(warn = -1)
  constructGraph <- cbind(names(datavalue),col, backTransparency)
  options(warn = oldw)
  graphs <- list()
  
  for(i in (nrow(constructGraph)):1)
  {
    graphs[[i]] <- amGraph(title = as.character(constructGraph[i,1]), lineThickness = 2, 
                           legendAlpha = 1, lineColor = as.character(constructGraph[i,2]), lineAlpha = 1, 
                           balloonText =  paste0("[[",as.character(constructGraph[i,1]),"lab]]"),
                           fillColors =  as.character(constructGraph[i,2]), 
                           fillAlphas = as.numeric(constructGraph[i,3]), 
                           valueField =  as.character(constructGraph[i,1]), 
                           bullet = "round", bulletAlpha = 0,
                           legendColor = as.character(constructGraph[i,2]))
  }
  
  data <- cbind(data,databullet)
  res <- amRadarChart(dataProvider = data, startDuration = 1,
                      categoryField = "labels", graphs = graphs) %>>% 
    addValueAxes(gridType = "circle", unit = "%")
  
  
  res <- amOptions(res, ...)
  res
}






