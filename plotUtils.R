
amHist <- function(x, plot = FALSE, ...){
  require(data.table)
  if (!missing(...)){
    resHist <- hist(x = x, plot = FALSE, ...)
  }else{
    resHist <- hist(x = x, plot = FALSE)
  }
  
  if (plot) {
    return (resHist)
  } else 
  {
    dt <- data.table(x = resHist$mids, y = resHist$counts)
    
    amSerialChart(theme = "light", categoryField = "x", creditsPosition = "top-right",
                  dataProvider = dt
    ) %>>% addGraph(balloonText = "[[category]]: <b>[[value]]</b>", type = "column",
                    valueField = "y", fillAlphas = .8, lineAlpha = .2
    ) %>>% addGraph( valueField = "y", type = "smoothedLine"
    ) %>>% setExport(position = "top-right"
    ) %>>% setChartCursor %>>% plot
  }
}
#amHist(rnorm(100), breaks = "Scott")

amPlot <- function(x, y, export = TRUE, weights, id,...){
  if ( missing(y) ){
    dt <- data.table( x = x )
    # dt <- data.table( x = rnorm(1000) )
    amSerialChart(theme = "light", creditsPosition = "top-right",
                  dataProvider = dt
    ) %>>% addGraph(balloonText = "value: <b>[[value]]</b>",
                    valueField = "x",lineAlpha = 1
    ) %>>% setExport(position = "top-right"
    ) %>>% setChartCursor %>>% plot
  } else {
    if ( length(x) != length(y) ){
      stop("error")
    }else{}
    if( missing( weights) ){
      weights <- rep(1, length(x))
      labelWeights <- "weights:<b>[[weights]]</b>"
    }else{labelWeights <- ""}
    if( missing( id) ){
      id <- 1 : length(x)
      labelID <- "Observation num. <b>[[id]]</b>"
    }else{labelID <- "<b>[[id]]</b>"}
    dt <- data.table( x = x, y = y, weights = weights, id = id  )
    balloonText <- paste0(labelID, "<br>",
                          "x:<b>[[x]]</b> y:<b>[[y]]</b><br>",
                          labelWeights, "<br>")
    amXYChart(theme = "light", dataProvider = dt
    ) %>>% addValueAxes(position = "bottom", axisAlpha = 0
    ) %>>% addGraph(balloonText = balloonText,
                    valueField = "weights", xField = "x", yField = "y",
                    bullet = "circle", lineAlpha = 0, maxBulletSize = 10
    ) %>>% setExport( enabled = export ) %>>% plot
  }
}
#amPlot( x = rnorm(100), y = rnorm(100), weights = rnorm(100, mean = 1) )
