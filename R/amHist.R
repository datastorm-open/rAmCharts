#' @title Plot an histogram
#' @description  amHist computes a histogram of the given data values.
#' If \code{plot = TRUE}, the resulting object of class \code{"histogram"} before it is returned.
#' 
#' @param x a vector of values for which the histogram is desired.
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
#' 
#' @return An object of class \linkS4class{AmChart}.
#' 
#' @example examples/amHist_examples.R
#'        
#' @rdname amHist     
#' @import data.table
#' @export
#' 
amHist <- function(x, ...) UseMethod("amHist")


#' @rdname amHist
#' @import pipeR
#' @import data.table
#' @export
#' 
amHist.numeric <- function(x, col = "gray", border = "gray",
                           freq = TRUE, plot = TRUE, labels = TRUE,
                           xlab, ylab, ylim, ...)
{
  .testNumeric(num = x)
  
  if (!missing(...)) {
    resHist <- graphics::hist(x = x, plot = FALSE, ...)
  } else {
    resHist <- graphics::hist(x = x, plot = FALSE)
  }
  .testestLogicalLength1(logi = plot)
  
  if (!plot) {
    return (resHist)
  } else {
    # check parameters
    .testCharacterLength1(char = border)
    .testCharacterLength1(char = col)
    .testestLogicalLength1(logi = labels)
    .testestLogicalLength1(logi = freq)
    
    if (!missing(xlab))
      .testCharacterLength1(char = xlab)
    
    if (!missing(ylab))
      .testCharacterLength1(char = ylab)
    
    amLabels <- ifelse(labels, "[[value]]", "")
    y <- if (freq) {
      resHist$counts
    } else {
      round(x = resHist$density, digits = 3)
    }
    
    if (!missing(ylim)) {
      .testLength(param = ylim, len = 2)
      .testNumeric(num = ylim)
    } else {
      ylim <- range(y*1.05, 0)
    }
    
    
    if (missing(ylab))
      ylab <- ifelse(test = !freq, yes = "Density", no = "Frequency")
    
    if (missing(xlab)) xlab <- deparse(substitute(x))
    
    dp <- .dataAmHist(resHist, y, col)
    .plotAmHist(dp = dp, amLabels = amLabels, ylim = ylim, ylab = ylab, xlab = xlab, border = border)
  }
}

.plotAmHist <- function(dp, amLabels, ylim, ylab, xlab, border)
{
  pipeR::pipeline(
    amSerialChart(theme = "light", categoryField = "x", columnSpacing = 0, 
                  creditsPosition = "bottom-right", dataProvider = dp,
                  columnWidth = 1, fillAlphas = 1, lineAlpha = 1),
    addGraph(balloonText = "<b>[[value]]</b> <br/> [[cut]] ", type = "column",
             valueField = "y", fillAlphas = .8, lineAlpha = 1,
             fillColorsField = "color", lineColor = border),
    addGraph(valueField = "y", type = "smoothedLine", lineColor = "black",
             balloonText = "", id = "graph-line"),
    addValueAxes(title = ylab, minimum = ylim[1], maximum = ylim[2]),
    setCategoryAxis(title = xlab)
  )
  
  
  #   if (scrollbar)
  #     chart <- setChartScrollbar(.Object = chart, graph = "graph-line", scrollbarHeight = 30,
  #                                backgroundAlpha = 0, offset = 60, autoGridCount = TRUE,
  #                                color = '#888888', dragIcon = "dragIconRectBigBlack",
  #                                oppositeAxis = FALSE, backgroundAlpha = 0, 
  #                                selectedBackgroundAlpha = 0.1, selectedBackgroundColor = '#888888',
  #                                graphFillAlpha = 0, selectedGraphFillAlpha = 0, graphLineAlpha = 0.8,
  #                                selectedGraphLineColor = '#888888', selectedGraphLineAlpha = 1)
}

.dataAmHist <- function (resHist, y, col)
{
  data_DT <- data.table(x = round(resHist$mids, 1), y = y, 
                        cut = paste0("(from ", round(resHist$breaks[-length(resHist$breaks)], 2),
                                     " to ", round(resHist$breaks[-1], 2), ")"))
  data_DT[, eval(parse(text = "color:=col"))]
}