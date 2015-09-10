
#' @import data.table
amHist <- function(x, main = NULL, xlab = NULL, ylab = NULL, ylim = NULL, plot = FALSE, col = NULL,
                   labels = FALSE, ...){
  if (!missing(...)){
    resHist <- hist(x = x, plot = FALSE, ...)
  }else{
    resHist <- hist(x = x, plot = FALSE)
  }
  
  amLabels <- ifelse(labels, "[[value]]", "")
  if (plot) {
    return (resHist)
  } else 
  {
    dt <- data.table(x = resHist$mids, y = resHist$counts, 
                     cut = paste0("(", paste(resHist$breaks[-length(resHist$breaks)], resHist$breaks[-1], sep = ", "), ")"))
    
    graph <- amSerialChart(theme = "light", categoryField = "x", creditsPosition = "top-right",
                           columnSpacing = 0, columnWidth = 1, fillAlphas = 1, lineAlpha = 0, 
                           dataProvider = dt
    ) %>>% addGraph(balloonText = "[[cut]]: <b>[[value]]</b>", type = "column",
                    valueField = "y", fillAlphas = .8, lineAlpha = .2, fillColors = col,
                    labelText = amLabels, showAllValueLabels = TRUE
    ) %>>% addGraph( valueField = "y", type = "smoothedLine", lineColor = "black"
    ) %>>% setExport(position = "top-right"
    ) %>>% setChartCursor
    
    if(!is.null(main)){
      graph <- addTitle(graph, text = main, size = 18)
    }
    
    if(!is.null(ylab) & !is.null(ylim)){
      graph <- addValueAxes(graph, title = ylab, minimum = ylim[1], maximum = ylim[length(ylim)])
    }
    
    if(!is.null(ylab) & is.null(ylim)){
      graph <- addValueAxes(graph, title = ylab)
    }
    
    if(!is.null(xlab)){
      graph <- setCategoryAxis(graph, title = xlab)
    }
    
    graph %>>% plot
  }
}

# amHist(rnorm(100), breaks = "Scott")
# amHist(rnorm(100), breaks = "Scott", labels = TRUE)
# amHist(rnorm(100), breaks = "Scott", main = "Histogram", ylab = "y-axis", xlab = "x-axis", col = "red")
# amHist(rnorm(100), breaks = "Scott", main = "Histogram", 
#        ylab = "y-axis", xlab = "x-axis", ylim = c(10, 15))