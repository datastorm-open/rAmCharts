#' @title Plotting histogram using rAmCharts
#' @description  amHist computes a histogram of the given data values.
#' If \code{plot = TRUE}, the resulting object of class \code{"histogram"} before it is returned.
#' @param x a vector of values for which the histogram is desired.
#' @param main \code{character}, title of the graph.
#' @param freq \code{logical}; if \code{TRUE}, the histogram graphic is a representation of frequencies,
#' the counts component of the result; if FALSE, probability densities,
#' component density, are plotted (so that the histogram has a total area of one).
#' Defaults to TRUE if and only if breaks are equidistant (and probability is not specified).
#' @param xlab,ylab \code{character}, labels of the axis.
#' @param ylim the range of y values with sensible defaults.
#' @param plot logical. If \code{TRUE} (default),
#' an histogram is plotted, otherwise a list of breaks and counts is returned.
#' In the latter case, a warning is used if (typically graphical)
#' arguments are specified that only apply to the \code{plot = TRUE} case.
#' @param col a colour to be used to fill the bars.
#' @param border a colour for the borders.
#' @param labels logical or character.
#' Additionally draw labels on top of bars, if not \code{FALSE};
#' if \code{TRUE}, draw the counts or rounded densities;
#' if labels is a \code{character}, draw itself.
#' @param ... further arguments and graphical parameters passed to plot.histogram
#' @examples
#' x <- rnorm(100)
#' 
#' # Default method
#' (object <- amHist(x = x))
#' print(object)
#' 
#' # Without plot
#' amHist(x = x, plot = FALSE)
#' 
#' # Specific options
#' amHist(x = x, border = "blue")
#' amHist(x = x, col = "lightblue")
#' amHist(x = x, col = "grey")
#' amHist(x = x, col = "gray")
#' amHist(x = x, freq = FALSE)
#' amHist(x = x, breaks = "Scott")
#' amHist(x = x, breaks = "Scott", labels = TRUE)
#' amHist(x = x, breaks = "Scott", main = "Histogram", ylab = "y-axis", xlab = "x-axis", col = "red")
#' amHist(x = x, breaks = "Scott", main = "Histogram", 
#'        ylab = "y-axis", xlab = "x-axis", ylim = c(10, 15))
#' amHist(rnorm(100), breaks = "Scott", main = "Histogram", 
#'        ylab = "y-axis", xlab = "x-axis")
#'        
#' @import data.table
#' @export
amHist <- function(x, main = "Histogram", col = "grey", border = "grey",
                   freq = TRUE, xlab = NULL, ylab = NULL, ylim = NULL,
                   plot = TRUE, labels = TRUE, ...)
{
  if (!requireNamespace(package = "pipeR")) {
    stop ("Please install the package pipeR for running this function")
  } else {}

  if (!missing(...)) {
    resHist <- graphics::hist(x = x, plot = FALSE, ...)
  } else {
    resHist <- graphics::hist(x = x, plot = FALSE)
  }
  
  if (!plot) {
    return (resHist)
  } else {
    amLabels <- ifelse(labels, "[[value]]", "")
    y <- if (freq) {
      resHist$counts
    } else {
      round(x = resHist$density, digits = 3)
    }
    
    if (is.null(ylim)) {
      ylim <- range(y*1.05, 0)
    } else {}
    
    if (is.null(ylab)) {
      ylab <- ifelse(test = !freq, yes = "Density", no = "Frequency")
    } else {}
    
    if (is.null(xlab)) {
      xlab <- "x"
    } else {}
    dp <- dataAmHist(resHist, y, col)
    plotAmHist(dp, amLabels, ylim, main, ylab, xlab, border)
  }
}

plotAmHist <- function(dp, amLabels, ylim, main, ylab, xlab, border) {
  pipeR::pipeline(
    amSerialChart(theme = "light", categoryField = "x", creditsPosition = "top-right",
                  columnSpacing = 0, columnWidth = 1, fillAlphas = 1, lineAlpha = 1,
                  dataProvider = dp),
    addGraph(balloonText = "<b>[[value]]</b> <br/> [[cut]] ", type = "column",
             valueField = "y", fillAlphas = .8, lineAlpha = 1, fillColorsField = "color",
             labelText = amLabels, showAllValueLabels = TRUE, lineColor = border),
    addGraph(valueField = "y", type = "smoothedLine", lineColor = "black",
             balloonText = ""),
    addValueAxes(title = ylab, minimum = ylim[1], maximum = ylim[2]),
    setCategoryAxis(title = xlab),
    addTitle(text = main, size = 18),
    setExport(position = "top-right"),
    setChartCursor()
  )
}

dataAmHist <- function (resHist, y, col)
{
  data_DT <- data.table(x = round(resHist$mids), y = y, 
                        cut = paste0("(from ", round(resHist$breaks[-length(resHist$breaks)], 2),
                                     " to ", round(resHist$breaks[-1], 2), ")"))
  data_DT[, color:=col]
}