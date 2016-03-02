#' @title Plotting wind using rAmCharts
#' @description  amWind computes a windplot of the given data values.
#' 
#' @param data \code{data.frame}, a dataframe which columns are series of values, 
#' from weakest wind (first column) to stronger wind (last column). See \link{data_wind}.
#' @param col  \code{character}, color(s) of serie(s) hexadecimal like "#00FF00".
#' @param backTransparency \code{numeric}, background transparency, between 0 and 1.
#' @param ... see \code{\link{amOptions}} for more options.
#' 
#' @example examples/amWind_examples.R
#'
#' @import data.table
#' @import pipeR
#' 
#' @export
#' @seealso 
#' #' @seealso 
#' \itemize{
#' \item{\url{https://dataknowledge.github.io/introduction_ramcharts/}}
#' \item{\link{amRadar}}
#' }
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

  databullet <- apply( rbind(names(data),data),2,function(x){paste0("<b>", as.numeric(x[-1]),
                    " </b>Observations <br><b>",round(as.numeric(x[-1])/sum(as.numeric(x[-1]))*100,2),
                    "%</b> of wind <b>",x[1],"</b>")})
  
  colnames(databullet) <- paste0(  colnames(databullet),"lab")
  
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
  col <- rev(col) 
  datavalue <- data[, (ncol(data)-1):1, drop = FALSE]
  if(is.null(col)){col <- ""}
  oldw <- getOption("warn")
  options(warn = -1)
  constructGraph <- cbind(names(datavalue),col, backTransparency)
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
                           bullet = "round", bulletAlpha = 0,
                           legendColor = as.character(constructGraph[i,2]))
  }
  
  data <- cbind(data,databullet)
  res <- amRadarChart(dataProvider = data, startDuration = 1,
                      categoryField = "labels", graphs = graphs) %>>% 
    addValueAxes(gridType = "circle")
  
  
  res <- amOptions(res, ...)
  res
}






