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
#' @param col a colour to be used to fill the bars. The default of NULL yields unfilled bars.
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
amHist <- function(x, main = "Histogram",
                   freq = TRUE, xlab = NULL, ylab = NULL,
                   ylim = NULL, plot = TRUE, col = NULL,
                   labels = TRUE, ...)
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
    
    plotAmHist(resHist, amLabels, y, ylim, main, ylab, xlab, col)
  }
}

plotAmHist <- function(resHist, amLabels, y, ylim, main, ylab, xlab, col) {
  pipeR::pipeline(
    amSerialChart(theme = "light", categoryField = "x", creditsPosition = "top-right",
                  columnSpacing = 0, columnWidth = 1, fillAlphas = 1, lineAlpha = 0),
    setDataProvider(data.table(x = resHist$mids, y = y, 
                               cut = paste0("(", paste(resHist$breaks[-length(resHist$breaks)],
                                                       resHist$breaks[-1], sep = ", "), ")"))),
    addGraph(balloonText = "[[cut]]: <b>[[value]]</b>", type = "column",
             valueField = "y", fillAlphas = .8, lineAlpha = .2, fillColors = col,
             labelText = amLabels, showAllValueLabels = TRUE),
    addGraph(valueField = "y", type = "smoothedLine", lineColor = "black"),
    addValueAxes(title = ylab, minimum = ylim[1], maximum = ylim[2]),
    setCategoryAxis(title = xlab),
    addTitle(text = main, size = 18),
    setExport(position = "top-right"),
    setChartCursor()
  )
}