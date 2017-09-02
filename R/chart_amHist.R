#' @title Plotting histogram using rAmCharts
#' @description  amHist computes a histogram of the given data values.
#' 
#' @param x \code{numeric}, a vector of values for which the histogram is desired.
#' @param freq \code{logical}, if \code{TRUE}, the histogram graphic is a representation of frequencies,
#' the counts component of the result; if FALSE, probability densities,
#' component density, are plotted (so that the histogram has a total area of one).
#' Defaults to TRUE if and only if breaks are equidistant (and probability is not specified).
#' @param xlab,ylab \code{character}, labels of the axis.
#' @param ylim \code{numeric}, the range of y values with sensible defaults.
#' @param plot \code{logical}, if \code{TRUE} (default),
#' an histogram is plotted, otherwise a list of breaks and counts is returned.
#' In the second case, a warning is used if (typically graphical)
#' arguments are specified that only apply to the \code{plot = TRUE} case.
#' @param col \code{character}, a color to be used to fill the bars.
#' @param border \code{character}, a color for the borders.
#' @param labels \code{logical}, set to TRUE to display labels. Default set to FALSE.
#' Additionally draw labels on top of bars.
#' if TRUE, draw the counts or rounded densities;
#' if labels is a \code{character}, draw itself.
#' @param control_hist (optional) named \code{list()} containing parameters to compute the histogram.
#' @param breaks \code{character} or \code{numeric} : one of:
#' - a vector giving the breakpoints between histogram cells,
#' - a function to compute the vector of breakpoints,
#' - a single number giving the number of cells for the histogram,
#' - a character string naming an algorithm to compute the number of cells,
#' - a function to compute the number of cells.
#' For more informations, see the 'hist' function.
#' @param breaks_precision \code{numeric} : precision of the intervals which are shown in the tooltips
#' @param ... see \code{\link{amOptions}} for more options.
#' 
#' @return An object of class \linkS4class{AmChart}.
#' 
#' @examples
#' amHist(x = rnorm(100))
#' 
#' \donttest{
#' # Other examples available which can be time consuming depending on your configuration.
#' 
#' x <- replicate(1000, {
#' if (round(runif(1))) {
#'   rnorm(1)
#' } else {
#'   rnorm(1, mean = 5)
#' }
#' })
#' 
#' 
#' # Without plot
#' amHist(x = x, plot = FALSE)
#' 
#' # With options
#' amHist(x = x, border = "blue")
#' amHist(x = x, col = "lightblue", control_hist = list(breaks = 100))
#' amHist(x = x, col = "grey")
#' amHist(x = x, col = "gray")
#' amHist(x = x, main = "Histogram", ylab = "y-axis", xlab = "x-axis", col = "red")
#' amHist(x = x, main = "Histogram", ylab = "y-axis", xlab = "x-axis", ylim = c(10, 15))
#' amHist(x = x, main = "Histogram", ylab = "y-axis", xlab = "x-axis")
#' 
#' # Options for computing the histogram
#' amHist(x = x, control_hist = list(breaks = "Scott"))
#' }
#' 
#' @seealso 
#' \itemize{
#' \item{\url{https://datastorm-open.github.io/introduction_ramcharts/}}
#' }
#'        
#' @rdname amHist     
#' @import data.table
#' @importFrom graphics hist
#' @export
#' 
amHist <- function(x, ...) UseMethod("amHist")


#' @rdname amHist
#' @import pipeR
#' @import data.table
#' @export
#' 
amHist.numeric <- function(x, col = "#1e90ff", border = "#1e90ff",
                           freq = TRUE, plot = TRUE, labels = FALSE,
                           xlab, ylab, ylim, control_hist, breaks = "Sturges", 
                           breaks_precision = 2, ...)
{
  .testNumeric(num = x)
  
  if (!missing(control_hist)) {
    resHist <- do.call(graphics::hist, c(list(x = x, plot = FALSE, breaks = breaks), control_hist))
  } else {
    resHist <- graphics::hist(x = x, plot = FALSE, breaks = breaks)
  }
  .testLogicalLength1(logi = plot)
  
  if (!plot) {
    return (resHist)
  } else {
    # check parameters
    .testCharacterLength1(char = border)
    .testCharacterLength1(char = col)
    
    .testLogicalLength1(logi = labels)
    
    .testLogicalLength1(logi = freq)
    
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
    
    dp <- .dataAmHist(resHist, y, col, breaks_precision)
    chart <- .plotAmHist(dp = dp, amLabels = amLabels, ylim = ylim,
                         ylab = ylab, xlab = xlab, border = border)
    amOptions(chart, ...)
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
             fillColorsField = "color", lineColor = border, labelText = amLabels),
    addValueAxes(title = ylab, minimum = ylim[1], maximum = ylim[2]),
    setCategoryAxis(title = xlab),
    setProperties(RType_ = "histogram")
  )
}

#' @import data.table
.dataAmHist <- function (resHist, y, col, breaks_precision)
{
  data_DT <- data.table(x = round(resHist$mids, 1), y = y, 
                        cut = paste0("(from ", round(resHist$breaks[-length(resHist$breaks)], breaks_precision),
                                     " to ", round(resHist$breaks[-1], breaks_precision), ")"))
  data_DT[, eval(parse(text = "color:=col"))]
}