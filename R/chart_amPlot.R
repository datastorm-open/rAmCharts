setClassUnion(name = "characterOrFactor", members = c("character", "factor"))

#' @title Plot serial data
#' 
#' @description  amPlot computes a plot of the given data values (can be a vectorn a dataframe
#' or a formula).
#' 
#' @param x the coordinates of points in the plot : \code{numeric},
#' \code{data.frame}, or \code{formula}.
#' @param y \code{numeric}, the y coordinates of points in the plot,
#' optional if x is an appropriate structure.
#' @param xlab \code{character},  label for x-axis.
#' @param ylab \code{character},  label for y-axis.
#' @param bullet \code{character}, point shape. Possible values are : "diamond", "square", 
#' "bubble",  "yError", "xError", "round", "triangleLeft", "triangleRight", "triangleUp", 
#' "triangleDown". Default set to "round".
#' @param type \code{character}, type of plot. Possible values are : "l" for a line, "sl" 
#' for a smoothed line (deprecated), "st" for steps, "p" for points, and "b" for line and points.
#' Default set to "p".
#' @param col either a \code{factor} or a \code{character}, default set to "gray".
#' @param fill_alphas a \code{numeric} between 0 and 1 for printed area.
#' @param weights \code{numeric}, weights for x/y charts only. Small values are prefered for lisibility.
#' @param id \code{numeric}, point id, for x/y charts only. Default 1:length(x).
#' @param precision \code{numeric}, precision you wish to display. Default set to 2.
#' @param dataDateFormat \code{character}, default set to NULL. Even if your chart parses dates,
#' you can pass them as strings in your dataframe - 
#' all you need to do is to set data date format and the chart will parse dates to date objects.
#' Check this page for available formats.
#' Please note that two-digit years (YY) as well as literal month names (MMM)  are NOT supported in this setting.
#' @param parseDates \code{logical}, default set to FALSE, if TRUE argument 
#' \code{dataDateFormat} has to be provided.
#' @param error \code{numeric}, only when type is "xError" "yError" default NULL,
#' @param xlim \code{numeric}, x range.
#' @param ylim \code{numeric}, y range.
#' @param cex \code{numeric}, bullet size.
#' @param lty \code{numeric}, line type (dashes).
#' @param lwd \code{numeric}, line width 
#' @param ... see \code{\link{amOptions}} for more options.
#' 
#' @return Return an Amchart.
#' 
#' @seealso 
#' \itemize{
#' \item{\url{https://datastorm-open.github.io/introduction_ramcharts/}}
#' }
#' 
#' @examples 
#' # 'numeric':
#' amPlot(x = rnorm(100))
#' 
#' # 'character':
#' start <- as.POSIXct('01-01-2015', format = '%d-%m-%Y')
#' end <- as.POSIXct('31-12-2015', format = '%d-%m-%Y')
#' date <- seq.POSIXt(from = start, to = end, by = 'day')
#' date <- format(date, '%m-%d-%Y')
#' 
#' y <- rnorm(length(date))
#' amPlot(x = date, y = y, type = 'l', parseDates = TRUE, dataDateFormat = "MM-DD-YYYY")
#' # notice that by default 'parseDates = FALSE'
#' 
#' # 'data.frame'
#' amPlot(iris, col = colnames(iris)[1:2], type = c("l", "st"), zoom = TRUE, legend = TRUE)
#' 
#' # 'formula':
#' amPlot(Petal.Length + Sepal.Length ~ Sepal.Width, data = iris, legend = TRUE, zoom = TRUE)
#' 
#' 
#' @import data.table
#' @rdname amPlot
#' 
#' @export
#' 
amPlot <- function(x, ...) UseMethod("amPlot")

#' @rdname amPlot
#' @export
#' 
amPlot.default <- function(x, ...) "Wrong class"

#' @rdname amPlot
#' 
#' @examples
#' \donttest{
#' # Other examples available which can be time consuming depending on your configuration.
#' library(data.table)
#' 
#' iris <- as.data.table(get("iris", "package:datasets"))
#' x <- rnorm(100)
#' 
#' # Simple scatter plot with title and color
#' # Also change type (set to "p" by default), avalaible "l", "sl", "st", "p", "b"
#' amPlot(x = x, main = "Title", col = "lightblue", type = "b")
#' 
#' x <- sort(rnorm(100))
#' y <- runif(100)
#' weights <- runif(100, 0, 15)
#' amPlot(x = x, y = y, weights = weights)
#' }
#' @import data.table
#' @import pipeR
#' @export
#' 
amPlot.numeric <- function(x, y,
                           bullet = c("round","diamond", "square", 
                                      "bubble",  "yError", "xError",
                                      "triangleLeft", "triangleRight",
                                      "triangleUp", "triangleDown"),
                           type = c("points", "line", "smoothedLine", "step", "both"),
                           col = "#0066cc", fill_alphas = 0,
                           weights = NULL, precision = 2, id, error, xlab, ylab,
                           lty, cex, lwd, xlim, ylim, ...)
{
  # check arguments validity
  # ---
  bullet <- match.arg(bullet)
  if (missing(lty)) lty <- 0
  if (missing(lwd)) lwd <- 1
  
  col <- as.factor(col)
  # check the color
  if (length(col) == 1) {
    col <- rep(col, times = length(x))
  } else {
    .testLength(col, length(x))
    levels(col) <- grDevices::topo.colors(nlevels(col))
    levels(col) <- substr(levels(col), 1, 7)
  }
  
  # check (and convert) the type
  if (length(type) == 1L && type == "sl") # exception to remove for the next submission
    type <- "smoothedLine"
  else 
    type <- match.arg(type)
  
  if (missing(y)) { # the user plot a simple line or point chart
    
    # define the dataProvider
    if (type == "points" && bullet %in% c("xError", "yError")) {
      if (missing(error)) error <- rep(1, length(x))
      .testNumeric(error)
      .testLength(error, length(x))
      dt <- data.table(x = x, cat = paste("obs.", 1:length(x)), error = error)
    } else {
      dt <- data.table(x = x, cat = paste("obs.", 1:length(x)))
    }
    
    dt <- cbind(dt, col = col)
    
    # define width of bullet
    if (missing(cex)) cex <- 1
    
    # define the graph object depending on the type
    graph_obj <- getGraph(type = type, col = col, fill_alphas = fill_alphas,
                          bullet = bullet,
                          title = deparse(substitute(x)),
                          cex = cex, lwd = lwd, lty = lty)
    
    # define axes label
    if (missing(xlab)) xlab <- "index"
    if (missing(ylab)) ylab <- deparse(substitute(x))
    
    # Add category axis at the left
    categoryAxis_obj <- if (!missing(xlim)) {
      .testNumeric(xlim)
      .testLength(xlim, 2)
      categoryAxis(title = xlab, position = "bottom", id = "x",
                   minimum = xlim[1], maximum = xlim[2])
    } else {
      categoryAxis(title = xlab, position = "bottom", id = "x")
    }
    
    # test fill_alphas value
    .testInterval(fill_alphas, 0, 1)
    
    # finally build the chart
    amSerialChart(categoryField = "cat", precision = precision) %>>%
      setCategoryAxis(title = xlab, position = "bottom", id = "x") %>>%
      addGraph(amGraph = graph_obj) %>>%
      setCategoryAxis(categoryAxis = categoryAxis_obj) %>>%
      setDataProvider(dataProvider = dt) %>>%
      (~ chart )
    
  } else if (is.numeric(y)) {
    # the user plot an XY chart
    
    if (length(x) != length(y)) stop("'x' and 'y' lengths differ")
    
    type <- match.arg(arg = type, choices = c("points", "line"))
    
    # axes label
    if (missing(xlab)) xlab <- deparse(substitute(x))
    if (missing(ylab)) ylab <- deparse(substitute(y))
    graphTitle <- deparse(substitute(y))
    
    # initialize dataProvider
    if (!missing(id)) {
      dt <- data.table(x = x, y = y, id = id, col = col)
      labelId  <- "Obs. <b>[[id]]</b><br>"
    } else {
      dt <- data.table(x = x, y = y, col = col)
      labelId  <- ""
    }
    
    if (!missing(weights)) {
      .testLength(weights, length(x))
      # width of bullets
      weights <- round(weights, precision)
      # weights <- sapply(weights, function (w) (w-min(weights)) / (max(weights) - min(weights))) * 10
      cex <- max(weights)
      dt <- cbind(dt, weights = weights)
      weighted <- TRUE
      labelWeights <- "weights:<b>[[weights]]</b>"
    } else {
      weighted <- FALSE
      labelWeights <- NULL
      if (missing(cex)) cex <- 0
    }
    # opacity of bullets
    bulletAlpha <- ifelse (!cex, 0, 1)
    
    balloonText <- paste0(labelId, "x:<b>[[x]]</b><br>y:<b>[[y]]</b><br>", labelWeights)
    
    # Add common valueAxis
    if (!missing(xlim)) {
      .testNumeric(xlim)
      .testLength(xlim, 2)
    } else {
      xlim <- range(x)
    }
    valueAxis_bottom <- valueAxis(title = xlab, position = "bottom", axisAlpha = 0,
                                  minimum = xlim[1], maximum = xlim[2])
    
    graph_obj <- getGraphXY(type = type, bullet = bullet, cex = cex, title = graphTitle,
                            lwd = lwd, lty = lty, bulletAlpha = bulletAlpha, col = col,
                            fill_alphas = fill_alphas, balloonText = balloonText, 
                            weighted = weighted)
    
    amXYChart(precision = precision) %>>%
      addGraph(amGraph = graph_obj) %>>%
      addValueAxis(valueAxis = valueAxis_bottom) %>>%
      setDataProvider(dataProvider = dt) %>>%
      (~ chart)
    
    # since simple line chart has no tooltip, we remove the latter for simple xy line chart
    # if (type == "line") chart <- setBalloon(.Object = chart, enabled = FALSE)
    
  } else {
    stop("Error in arguments x or y")
  }
  
  # Add common valueAxis at the left
  if (!missing(ylim)) {
    .testNumeric(ylim)
    .testLength(ylim, 2)
    valueAxis_left <- valueAxis(title = ylab, position = "left", axisAlpha = 0, id = "y",
                                minimum = ylim[1], maximum = ylim[2])
  } else if (!missing(y)) {
    ylim <- range(y)
    valueAxis_left <- valueAxis(title = ylab, position = "left", axisAlpha = 0, id = "y",
                                minimum = ylim[1], maximum = ylim[2])
  } else {
    valueAxis_left <- valueAxis(title = ylab, position = "left", axisAlpha = 0, id = "y")
  }
  
  chart <- addValueAxis(.Object = chart, valueAxis = valueAxis_left)
  
  # return the object
  amOptions(chart, ...)
}

#' @rdname amPlot
#' @import data.table
#' @import pipeR
#' @export
#' 
amPlot.character <- function(x, y,
                             bullet = c("round","diamond", "square", 
                                        "bubble",  "yError", "xError",
                                        "triangleLeft", "triangleRight",
                                        "triangleUp", "triangleDown"),
                             type = c("points", "line", "smoothedLine", "step", "both"),
                             col = "#0066cc", fill_alphas = 0,
                             weights = NULL,
                             precision = 2,
                             parseDates = FALSE, dataDateFormat,
                             id, error, xlab, ylab,
                             lty, cex, lwd, xlim, ylim, ...)
{
  # check arguments validity
  # ---
  bullet <- match.arg(bullet)
  if (missing(lty)) lty <- 0
  if (missing(lwd)) lwd <- 1
  
  col <- as.factor(col)
  # check the color
  if (length(col) == 1) {
    col <- rep(col, times = length(x))
  } else {
    .testLength(col, length(x))
    levels(col) <- grDevices::topo.colors(nlevels(col))
    levels(col) <- substr(levels(col), 1, 7)
  }
  
  if (is.numeric(y)) {
    # the user plot a simple line or point chart by referencing x axis
    
    if (length(x) != length(y)) stop("'x' and 'y' lengths differ")
    
    # check (and convert) the type 
    if (length(type) == 1L && type == "sl") # exception to remove for the next submission
      type <- "smoothedLine"
    else 
      type <- match.arg(type)
    
    # define the dataProvider
    if (type == "points" && bullet %in% c("xError", "yError")) {
      if (missing(error)) error <- rep(1, length(x))
      .testNumeric(error)
      .testLength(error, length(x))
      dt <- data.table(x = y, cat = x, error = error)
    } else {
      dt <- data.table(x = y, cat = x)
    }
    
    dt <- cbind(dt, col = col)
    
    # define width of bullet
    if (missing(cex)) cex <- 1
    
    # define the graph object depending on the type
    graph_obj <- getGraph(type = type, col = col, fill_alphas = fill_alphas, 
                          bullet = bullet, title = deparse(substitute(y)),
                          cex = cex, lwd = lwd, lty = lty)
    
    # define axes label
    if (missing(xlab)) xlab <- "index"
    if (missing(ylab)) ylab <- deparse(substitute(y))
    
    if (parseDates) {
      stopifnot(!missing(dataDateFormat))
      .testCharacter(dataDateFormat)
      chart <- amSerialChart(categoryField = "cat",
                             precision = precision,
                             dataDateFormat = dataDateFormat)
    } else {
      chart <- amSerialChart(categoryField = "cat", precision = precision)
    }
    
    # test fill_alphas value
    .testInterval(fill_alphas, 0, 1)
    
    # finally build the chart
    chart %>>%
      addGraph(amGraph = graph_obj) %>>%
      setCategoryAxis(title = xlab, position = "bottom", id = "x", parseDates = parseDates) %>>%
      setDataProvider(dataProvider = dt) %>>%
      (~ chart )
    
  } else {
    stop("Error of argument y")
  }
  
  # Add common valueAxis at the left
  if (!missing(ylim)) {
    .testNumeric(ylim)
    .testLength(ylim, 2)
    valueAxis_left <- valueAxis(title = ylab, position = "left", axisAlpha = 0, id = "y",
                                minimum = ylim[1], maximum = ylim[2])
  } else if (!missing(y)) {
    ylim <- range(y)
    valueAxis_left <- valueAxis(title = ylab, position = "left", axisAlpha = 0, id = "y",
                                minimum = ylim[1], maximum = ylim[2])
  } else {
    valueAxis_left <- valueAxis(title = ylab, position = "left", axisAlpha = 0, id = "y")
  }
  
  chart <- addValueAxis(.Object = chart, valueAxis = valueAxis_left)
  
  # return the object
  amOptions(chart, ...)
}


#' @rdname amPlot
#' 
#' @import data.table
#' @import pipeR
#' @export
#' 
amPlot.factor <- function(x, y, bullet = "round", type = "p", col = "gray", 
                          weights = NULL, precision = 2, 
                          parseDates = FALSE, dataDateFormat = NULL,
                          id, error, xlab, ylab,
                          lty, cex, lwd, xlim, ylim, ...)
{
  amPlot.character(x = as.character(x), y = y, bullet = bullet, type = type, col = col, 
                   weights = weights, precision = precision,
                   parseDates = parseDates, dataDateFormat = dataDateFormat,
                   id = id, error = error, xlab = xlab, ylab = ylab,
                   lty = lty, cex = cex, lwd = lwd,
                   xlim = xlim, ylim = xlim, ...)
}

#' @rdname amPlot
#' 
#' @param columns (optional) either a vector of \code{character} containing
#' the names of the series to draw, or a \code{numeric} vector of indices.
#' By default all numeric columns will be drawn.
#' 
#' @import pipeR
#' @import data.table
#'
#' @export
#' 
amPlot.data.frame <- function(x, columns, type = "l", precision = 2, xlab, ylab, fill_alphas = 0, ...)
{
  if (missing(ylab)) ylab <- deparse(substitute(x))
  if (missing(xlab)) xlab <- "index"
  
  # test fill_alphas value
  .testInterval(fill_alphas, 0, 1)
  
  if (missing(columns)) {
    columns <- sapply(x, is.numeric)
    columns <- names(columns)[columns]
  } else {}
  
  if (is.character(columns)) columns <- which(colnames(x) %in% columns)
  if (!is.data.table(x)) x <- as.data.table(x)
  x <- x[, eval(columns), with = FALSE]
  
  if (ncol(x) == 1) x <- x[[1]]
  
  if (is.data.frame(x)) {
    names <- colnames(x)
    
    # check the type
    if (length(type) > 1 && length(type) != ncol(x))
      stop("Invalid argument type")
    
    type <- sapply(X = type, FUN = function (t) {
      # check (and convert) the type
      if (t == "sl") # exception to remove for the next submission
        return ("smoothedLine")
      else 
        return (match.arg(t, c("line", "step")))
    })
    
    if (length(type) == 1) type <- rep(type, ncol(x))
    
    # if type has been created from sapply, it is a named vector
    names(type) <- NULL
    
    graphs_ls <- lapply(1:ncol(x), FUN = function (i) {
      graph(balloonText = "value: <b>[[value]]</b>",
            title = names[i], valueField = names[i],
            lineAlpha = 1, type = type[i], bullet = "round",
            fill_alphas = fill_alphas,
            bulletAlpha = 0)
    })
    
    x <- cbind(x, amCategory = paste("Obs.", 1:nrow(x)))
    
    amSerialChart(precision = precision) %>>%
      setCategoryField(categoryField = "amCategory") %>>%
      setCategoryAxis(title = xlab, position = "bottom", id = "x") %>>%
      addValueAxis(title = ylab, position = "left", id = "y") %>>%
      setGraphs(graphs = graphs_ls) %>>%
      setDataProvider(dataProvider = x) %>>%
      (~ chart)
    
  } else {
    .testNumeric(x)
    chart <- amPlot(x = x, type = type)
  }
  
  amOptions(chart, ...)
}

#' @rdname amPlot
#' 
#' @param data dataset
#' 
#' @import pipeR
#' @export
#' 
amPlot.formula <- function (x, data, type = "p", fill_alphas = 0, ...)
{
  y_name <- all.vars(x[-3]) # subset variables in the lhs
  x_name <- all.vars(x[-2]) # subset variables in the rhs
  if (length(y_name) == 1) {
    y <- data[[eval(y_name)]]
    assign(y_name, y)
    chart <- eval(parse(text = paste0("amPlot(x = data[[eval(x_name)]], y = ", y_name,
                                      ", xlab = eval(x_name), ylab = eval(y_name), type = type, fill_alphas = fill_alphas, ...)")))
  } else {
    i <- 1
    y <- data[[eval(y_name[i])]]
    assign(y_name[i], y)
    chart <- eval(parse(text = paste0("amPlot(x = data[[eval(x_name)]], y = ", y_name[i],
                                      ", xlab = eval(x_name), ylab = 'multiple series', type = type, fill_alphas = fill_alphas, ...)")))
    i <- i + 1
    while(i <= length(y_name)) {
      chart <- chart %>>%
        amLines(y = data[[eval(y_name[i])]], title = eval(y_name[i]), 
                type = type, fill_alphas = fill_alphas)
      i <- i + 1
    }
  }
  amOptions(chart, ...)
}


getGraph <- function (type, col, bullet, cex, lwd, lty, title, fill_alphas)
{
  if (type == "points" && bullet %in% c("xError", "yError"))
    graph_obj <- graph(balloonText = "value: <b>[[value]]</b>", valueField = "x",
                       lineAlpha = 0, fillAlphas = fill_alphas,
                       errorField = "error", title = title,
                       bulletAxis = "y", bullet = bullet, bulletSize = cex)
  else if (type == "points")
    graph_obj <- graph(balloonText = "value: <b>[[value]]</b>", valueField = "x",
                       lineAlpha = 0, fillAlphas = fill_alphas,
                       bullet = bullet, bulletSize = cex, title = title)
  else if (type == "both")
    graph_obj <- graph(balloonText = "value: <b>[[value]]</b>", valueField = "x",
                       lineAlpha = 1, fillAlphas = fill_alphas,
                       lineThickness = lwd, title = title,
                       dashLength = lty, bullet = bullet, bulletSize = cex, type = "smoothedLine")
  else if(type %in% c("line", "smoothedLine", "step"))
    graph_obj <- graph(balloonText = "value: <b>[[value]]</b>", valueField = "x", title = title,
                       lineAlpha = 1, fillAlphas = fill_alphas,
                       dashLength = lty, lineThickness = lwd, type = type,
                       bullet = "round", bulletAlpha = 0)
  else
    graph_obj <- graph(balloonText = "value: <b>[[value]]</b>", valueField = "x", title = title,
                       lineAlpha = 1, fillAlphas = fill_alphas, 
                       dashLength = lty, lineThickness = lwd, type = type)
  
  if (nlevels(col) == 1) 
    setProperties(.Object = graph_obj, lineColor = levels(col))
  else
    setProperties(.Object = graph_obj, lineColorField = "col")
}

getGraphXY <- function (type, colorField, bullet, cex, lwd, lty, col,
                        bulletAlpha, balloonText, weighted, title, fill_alphas)
{
  graph_obj <- switch (type,
                       "points" = {
                         graph(balloonText = balloonText, valueField = "weights",
                               xField = "x", yField = "y", lineAlpha = 0,
                               title = title, bullet = bullet)
                       },
                       "smoothedLine" = {
                         graph(balloonText = balloonText, valueField = "weights", title = title,
                               xField = "x", yField = "y", bullet = bullet, fillAlphas = fill_alphas,
                               lineThickness = lwd, dashLength = lty, bulletAlpha = bulletAlpha)
                       },
                       "line" = {
                         graph(balloonText = balloonText, valueField = "weights", title = title,
                               xField = "x", yField = "y", bullet = "round", bulletAlpha = 0,
                               fillAlphas = fill_alphas, lineThickness = lwd, dashLength = lty)
                       })
  
  if (weighted) 
    graph_obj <- setProperties(graph_obj, maxBulletSize = cex)
  else
    graph_obj <- setProperties(graph_obj, bulletSize = cex)
  
  if (nlevels(col) == 1) 
    setProperties(.Object = graph_obj, lineColor = levels(col))
  else
    setProperties(.Object = graph_obj, lineColorField = "col")
}

#' @title amLines adds a serie to a graph.
#' @description amLines adds a new serie to an existing serial chart.
#'
#' @param chart \linkS4class{AmChart}. Chart you wish to add the new serie.
#' @param x \code{numeric}, equivalent to y, deprecated.
#' @param y \code{numeric}.
#' @param type (optionnal) \code{character}. Possible values are : "l" for line, 
#' "p" for points, "sl" for smoothed line. 
#' @param col \code{character}, color of the new serie.
#' @param fill_alphas a \code{numeric} between 0 and 1 for printed area.
#' @param title \code{character}, name of the new serie, used when legend is enabled.
#' 
#' @examples
#' require(pipeR)
#' amPlot(x = rnorm(100), type = 'sl') %>>%
#'   amLines(x = rnorm(100), type = "p")
#'   
#' \donttest{
#' amPlot(x = rnorm(100), type = 'sl') %>>%
#'   amLines(x = rnorm(100), col = "blue") %>>%
#'   amLines(x = rnorm(100), type = "sl") %>>%
#'   amLines(x = rnorm(100), type = "p")
#' 
#' # For an XY chart
#' x <- sort(rnorm(100))
#' y1 <- rnorm(100, sd = 10)
#' y2 <- rnorm(100, sd = 10)
#' y3 <- rnorm(100, sd = 10)
#' amPlot(x = x, y = y1) %>>%
#'   amLines(x = y2, col = "blue") %>>%
#'   amLines(x = y3, type = "p")
#' }
#' 
#' @rdname amLines
#' @export
#' 
#' @note It is supposed here that x or y corresponds to the y-axis, and the x-axis
#' is automatically linked to the x values of the chart "chart". That is why it makes
#' sense to give the y argument.
#' 
amLines <- function(chart, x = NULL, y = NULL,
                    type = c("points", "line", "smoothedLine"),
                    col = "#0066cc", title, fill_alphas = 0)
{
  
  
  if (!is.null(x) && !is.null(y))
    stop("Please use only y, x is deprecated.")
  
  
  if (is.null(x)) {
    if (is.null(y)) {
      stop("y is necessary")
    } else {
      if (missing(title)) title <- deparse(substitute(y))
      x <- y
    }
  } else {
    if (missing(title)) title <- deparse(substitute(x))
  }
  
  # check the arguments
  stopifnot(is(chart, "AmChart"))
  .testNumeric(x)
  
  if (length(type) == 1L && type == "sl") # exception to remove for the next submission
    type <- "smoothedLine"
  else 
    type <- match.arg(type)
  
  lineAlpha <- ifelse(type == "points", yes = 0, no = 1)
  
  if (!missing(col)) {
    .testCharacterLength1(col)
  } else {
    col <- ""
  }
  
  # test the length of the vector
  dataProvider <- chart@dataProvider
  l <- length(dataProvider)
  .testLength(x, l)
  
  # define the new name for the serie
  # here we suppose that each element of the list have the same names
  # consequently this method won't work if the dataProvider has been set
  # with NA values for the first line
  names <- names(dataProvider[[1]])
  i <- 1
  repeat {
    name <- paste0("amLines", i)
    if (!(name %in% names)) break
    else i <- i + 1
  }
  
  # append the new element to the dataProvider
  chart@dataProvider <- lapply(1:l, function(i) {
    dataProvider[[i]][[name]] <- x[i]
    dataProvider[[i]]
  })
  
  balloonText <- ifelse(chart@type == "xy",  paste0("x:<b>[[x]]</b><br>", title,": <b>[[", name, "]]</b><br>"), 
                        paste0(title,": <b>[[", name, "]]</b><br>"))
  
  # initialize the graph object
  graph_obj <- graph(title = title, valueField = name,
                     lineAlpha = lineAlpha, fillAlphas = fill_alphas,
                     lineColor = col, 
                     balloonText = balloonText)
  
  # the field where to find the new values depend on the chart type
  if (chart@type != "serial")
    graph_obj <- setProperties(.Object = graph_obj, xField = "x", yField = name)
  
  if (type == "points")
    graph_obj <- setProperties(.Object = graph_obj, bullet = "round", maxBulletSize = 5)
  if(type != "points")
    graph_obj <- setProperties(.Object = graph_obj, bullet = "round", maxBulletSize = 5, bulletAlpha = 0)
  # set the type if necessary
  if (type == "smoothedLine")
    graph_obj <- setType(.Object = graph_obj, type = "smoothedLine")
  
  # add the graph
  addGraph(.Object = chart, amGraph = graph_obj)
}


