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
#' for a smoothed line, "st" for steps, "p" for points, and "b" for line and points.
#' Default set to "p".
#' @param col either a \code{factor} or a \code{character}, default set to "gray".
#' @param weights \code{numeric}, weights for x/y charts only. Default set to rep(1, x).
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
#' @example examples/amPlot_examples.R
#' 
#' @seealso 
#' \itemize{
#' \item{\url{https://dataknowledge.github.io/introduction_ramcharts/}}
#' }
#' 
#' @import data.table
#' @rdname amPlot
#' @export
#' 
amPlot <- function(x, ...) UseMethod("amPlot")

#' @rdname amPlot
#' @export
#' 
amPlot.default <- function(x, ...) "Wrong class"

#' @rdname amPlot
#' @example examples/amLines_examples.R
#' 
#' @import data.table
#' @import pipeR
#' @export
#' 
amPlot.numeric <- function(x, y, bullet = "round", type = "p", col = "gray", 
                           weights = NULL, precision = 2, id, error, xlab, ylab,
                           lty, cex, lwd, xlim, ylim, ...)
{
  # check arguments validity
  # ---
  bullet <- amCheck_bullet(bullet)
  if (missing(lty)) lty <- 0
  if (missing(lwd)) lwd <- 1
  
  col <- as.factor(col)
  # check the color
  if (length(col) == 1) {
    col <- rep(col, times = length(x))
  } else {
    stopifnot(length(col) == length(x))
    levels(col) <- grDevices::topo.colors(nlevels(col))
    levels(col) <- substr(levels(col), 1, 7)
  }
  
  if (missing(y)) {
    # the user plot a simple line or point chart
    
    # check (and convert) the type 
    type <- amCheck_type(type)
    
    # define the dataProvider
    if (type == "points" && bullet %in% c("xError", "yError")) {
      if (missing(error)) error <- rep(1, length(x))
      stopifnot(is.numeric(error) && length(error) == length(x))
      dt <- data.table(x = x, cat = paste("obs.", 1:length(x)), error = error)
    } else {
      dt <- data.table(x = x, cat = paste("obs.", 1:length(x)))
    }
    
    dt <- cbind(dt, col = col)
    
    # define width of bullet
    if (missing(cex)) cex <- 1
    
    # define the graph object depending on the type
    graph_obj <- getGraph(type = type, col = col, bullet = bullet,
                          title = deparse(substitute(x)),
                          cex = cex, lwd = lwd, lty = lty)
    
    # define axes label
    if (missing(xlab)) xlab <- "index"
    if (missing(ylab)) ylab <- deparse(substitute(x))
    
    # Add category axis at the left
    categoryAxis_obj <- if (!missing(xlim)) {
      stopifnot(is.numeric(xlim) && length(xlim) == 2)
      categoryAxis(title = xlab, position = "bottom", id = "x",
                   minimum = xlim[1], maximum = xlim[2])
    } else {
      categoryAxis(title = xlab, position = "bottom", id = "x")
    }
    
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
    
    # check (and convert) the type 
    type <- amCheck_type(type, valid = c("p", "l"))
    
    # axes label
    if (missing(xlab)) xlab <- deparse(substitute(x))
    if (missing(ylab)) ylab <- deparse(substitute(y))
    graphTitle <- deparse(substitute(y))
    
    # width of bullets
    if (missing(cex) && missing(weights)) cex <- 0
    else if (missing(cex) && !missing(weights)) cex <- max(weights)
    
    # opacity of bullets
    bulletAlpha <- ifelse (!cex, 0, 1)
    
    # initialize dataProvider
    if (!missing(id)) {
      dt <- data.table(x = x, y = y, id = id, col = col)
      labelId  <- "Obs. <b>[[id]]</b>"
    } else {
      dt <- data.table(x = x, y = y, col = col)
      labelId  <- ""
    }
    
    if (!missing(weights)) {
      stopifnot(length(weights) == length(x))
      # width of bullets
      weights <- round(weights, precision)
      dt <- cbind(dt, weights = weights)
      weighted <- TRUE
      labelWeights <- "weights:<b>[[weights]]</b>"
    } else {
      weighted <- FALSE
      labelWeights <- NULL
    }
    
    balloonText <- paste0(labelId, "<br>",
                          "x:<b>[[x]]</b> <br> y:<b>[[y]]</b><br>",
                          labelWeights, "<br>")
    
    # Add common valueAxis
    valueAxis_bottom <- if (!missing(xlim)) {
      stopifnot(is.numeric(xlim) && length(xlim) == 2)
      valueAxis(title = xlab, position = "bottom", axisAlpha = 0,
                minimum = xlim[1], maximum = xlim[2])
    } else {
      valueAxis(title = xlab, position = "bottom", axisAlpha = 0)
    }
    
    graph_obj <- getGraphXY(type = type, bullet = bullet, cex = cex, title = graphTitle,
                            lwd = lwd, lty = lty, bulletAlpha = bulletAlpha,
                            balloonText = balloonText, weighted = weighted)
    
    amXYChart(precision = precision) %>>%
      addGraph(amGraph = graph_obj) %>>%
      addValueAxis(valueAxis = valueAxis_bottom) %>>%
      setDataProvider(dataProvider = dt) %>>%
      (~ chart)
    
  } else {
    stop("Error in arguments x or y")
  }
  
  # Add common valueAxis at the left
  valueAxis_left <- if (!missing(ylim)) {
    stopifnot(is.numeric(ylim) && length(ylim) == 2)
    valueAxis(title = ylab, position = "left", axisAlpha = 0,
              minimum = ylim[1], maximum = ylim[2], id = "y")
  } else {
    valueAxis(title = ylab, position = "left", axisAlpha = 0, id = "y")
  }
  
  chart <- addValueAxis(.Object = chart, valueAxis = valueAxis_left)
  
  # return the object
  amOptions(chart, ...)
}

#' @rdname amPlot
#' @example examples/amPlot_examples.R
#' 
#' @import data.table
#' @import pipeR
#' @export
#' 
amPlot.character <- function(x, y, bullet = "round", type = "p", col = "gray", 
                            weights = NULL, precision = 2,
                             parseDates = FALSE, dataDateFormat,
                             id, error, xlab, ylab,
                             lty, cex, lwd, xlim, ylim, ...)
{
  # check arguments validity
  # ---
  bullet <- amCheck_bullet(bullet)
  if (missing(lty)) lty <- 0
  if (missing(lwd)) lwd <- 1
  
  col <- as.factor(col)
  # check the color
  if (length(col) == 1) {
    col <- rep(col, times = length(x))
  } else {
    stopifnot(length(col) == length(x))
    levels(col) <- grDevices::topo.colors(nlevels(col))
    levels(col) <- substr(levels(col), 1, 7)
  }
  
  if (is.numeric(y)) {
    # the user plot a simple line or point chart by referencing x axis
    
    if (length(x) != length(y)) stop("'x' and 'y' lengths differ")
    
    # check (and convert) the type 
    type <- amCheck_type(type)
    
    # define the dataProvider
    if (type == "points" && bullet %in% c("xError", "yError")) {
      if (missing(error)) error <- rep(1, length(x))
      stopifnot(is.numeric(error) && length(error) == length(x))
      dt <- data.table(x = y, cat = x, error = error)
    } else {
      dt <- data.table(x = y, cat = x)
    }
    
    dt <- cbind(dt, col = col)
    
    # define width of bullet
    if (missing(cex)) cex <- 1
    
    # define the graph object depending on the type
    graph_obj <- getGraph(type = type, col = col, bullet = bullet,
                          title = deparse(substitute(x)),
                          cex = cex, lwd = lwd, lty = lty)
    
    # define axes label
    if (missing(xlab)) xlab <- "index"
    if (missing(ylab)) ylab <- deparse(substitute(x))
    
    if (parseDates) {
      stopifnot(!missing(dataDateFormat) && is.character(dataDateFormat))
      amSerialChart(categoryField = "cat", precision = precision, dataDateFormat = dataDateFormat) %>>%
        (~ chart )
    } else {
      amSerialChart(categoryField = "cat", precision = precision) %>>%
        (~ chart )
    }
    
    
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
  valueAxis_left <- if (!missing(ylim)) {
    stopifnot(is.numeric(ylim) && length(ylim) == 2)
    valueAxis(title = ylab, position = "left", axisAlpha = 0,
              minimum = ylim[1], maximum = ylim[2], id = "y")
  } else {
    valueAxis(title = ylab, position = "left", axisAlpha = 0, id = "y")
  }
  
  chart <- addValueAxis(.Object = chart, valueAxis = valueAxis_left)
  
  # return the object
  amOptions(chart, ...)
}


#' @rdname amPlot
#' @example examples/amPlot_examples.R
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
#' @param columns (optional) either a vector of \code{character} containing
#' the names of the series to draw, or a \code{numeric} vector of indices.
#' By default all numeric columns will be drawn.
#' 
#' @import pipeR
#' @import data.table
#'
#' @export
#' 
amPlot.data.frame <- function(x, columns, type = "l", precision = 2, xlab, ylab, ...)
{
  if (missing(ylab)) ylab <- deparse(substitute(x))
  if (missing(xlab)) xlab <- "index"
  
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
    
    type <- sapply(type, amCheck_type, valid = c("l", "sl", "st"))
    
    if (length(type) == 1) type <- rep(type, ncol(x))
    
    # if type has been created from sapply, it is a named vector
    names(type) <- NULL
    
    graphs_ls <- lapply(1:ncol(x), FUN = function (i) {
      graph(balloonText = "value: <b>[[value]]</b>",
            title = names[i], valueField = names[i],
            lineAlpha = 1, type = type[i])
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
    stopifnot(is.numeric(x))
    chart <- amPlot(x = x, type = type)
  }
  
  amOptions(chart, ...)
}

#' @rdname amPlot
#' @param data dataset
#' 
#' @import pipeR
#' @export
#' 
amPlot.formula <- function (x, data, ...)
{
  y_name <- all.vars(x[-3]) # subset variables in the lhs
  x_name <- all.vars(x[-2]) # subset variables in the rhs
  if (length(y_name) == 1) {
    chart <- amPlot(x = data[[eval(x_name)]], y = data[[eval(y_name)]],
                    xlab = eval(x_name), ylab = eval(y_name), ...)
  } else {
    i <- 1
    chart <- amPlot(x = data[[eval(x_name)]], y = data[[eval(y_name[i])]],
                    xlab = eval(x_name), ylab = "multiple series", ...)
    repeat {
      chart <- chart %>>%
        amLines(x = data[[eval(y_name[i])]], type = "p", title = eval(y_name[i]))
      if (i == length(y_name)) break
      else i <- i + 1
    }
  }
  chart
}

amCheck_type <- function(type, valid = c("l", "sl", "st", "p", "b"))
{
  stopifnot(type %in% valid)
  
  switch(type,
         "l" = "line",
         "sl" = "smoothedLine",
         "st" = "step",
         "p" = "points",
         "b" = "both",
         stop("Invalid type"))
}

amCheck_bullet <- function (bullet)
{
  validBullets <- c("diamond", "square", "bubble", "yError",
                    "xError", "round", "triangleLeft", "triangleRight",
                    "triangleUp", "triangleDown")
  if (!(bullet %in% validBullets)) stop("Invalid bullet name")
  
  bullet
}

getGraph <- function (type, col, bullet, cex, lwd, lty, title)
{
  if (type == "points" && bullet %in% c("xError", "yError"))
    graph_obj <- graph(balloonText = "value: <b>[[value]]</b>", valueField = "x",
                       lineAlpha = 0, errorField = "error", title = title,
                       bulletAxis = "y", bullet = bullet, bulletSize = cex)
  else if (type == "points")
    graph_obj <- graph(balloonText = "value: <b>[[value]]</b>", valueField = "x",
                       lineAlpha = 0, bullet = bullet, bulletSize = cex, title = title)
  else if (type == "both")
    graph_obj <- graph(balloonText = "value: <b>[[value]]</b>", valueField = "x",
                       lineAlpha = 1, lineThickness = lwd, title = title,
                       dashLength = lty, bullet = bullet, bulletSize = cex, type = "smoothedLine")
  else
    graph_obj <- graph(balloonText = "value: <b>[[value]]</b>", valueField = "x", title = title,
                       lineAlpha = 1, dashLength = lty, lineThickness = lwd, type = type)
  
  if (nlevels(col) == 1) 
    setProperties(.Object = graph_obj, lineColor = levels(col))
  else
    setProperties(.Object = graph_obj, lineColorField = "col")
}

getGraphXY <- function (type, colorField, bullet, cex, lwd, lty,
                        bulletAlpha, balloonText, weighted, title)
{
  graph_obj <- switch (type,
                       "points" = {
                         graph(balloonText = balloonText, valueField = "weights",
                               xField = "x", yField = "y", lineAlpha = 0,
                               title = title, lineColorField = "col", bullet = bullet)
                       },
                       "smoothedLine" = {
                         graph(balloonText = balloonText, valueField = "weights",
                               lineColorField = "col", title = title,
                               xField = "x", yField = "y", bullet = bullet,
                               lineThickness = lwd, dashLength = lty, bulletAlpha = bulletAlpha)
                       },
                       "line" = {
                         graph(balloonText = balloonText, valueField = "weights",
                               lineColorField = "col", title = title,
                               xField = "x", yField = "y", bullet = bullet,
                               lineThickness = lwd, dashLength = lty, bulletAlpha = bulletAlpha)
                       })
  
  if (weighted) setProperties(graph_obj, maxBulletSize = cex)
  else setProperties(graph_obj, bulletSize = cex)
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
#' @param title \code{character}, name of the new serie, used when legend is enabled.
#' 
#' @example examples/amPlot_examples.R
#' 
#' @rdname amLines
#' @export
#' 
#' @note It is supposed here that x or y corresponds to the y-axis, and the x-axis
#' is automatically linked to the x values of the chart "chart". That is why it makes
#' sense to give the y argument. 

amLines <- function(chart, x = NULL, y = NULL, type, col, title)
{

  
  if(!is.null(x) && !is.null(y))
  {
    stop("Please use only y, x is deprecated.")
  }
      
      
  if(is.null(x))
  {
    if(is.null(y))
    {
      stop("y is necessary")
    }else{
      if (missing(title)) title <- deparse(substitute(y))
      x <- y
    }
  }else{
  
  if (missing(title)) title <- deparse(substitute(x))
  }
  # check the arguments
  stopifnot(is(chart, "AmChart"))
  stopifnot(is.numeric(x))

  
  type <- if (missing(type)) "line"
  else amCheck_type(type = type, valid = c("l", "p", "sl"))
  
  lineAlpha <- ifelse(type == "points", yes = 0, no = 1)
  
  if (!missing(col))
    stopifnot(is.character(col) && length(col) == 1)
  else
    col <- ""
  
  # test the length of the vector
  dataProvider <- chart@dataProvider
  l <- length(dataProvider)
  stopifnot(length(x) == l)

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
  
  # initialize the graph object
  graph_obj <- graph(title = title, valueField = name, lineAlpha = lineAlpha, lineColor = col)
  
  # the field where to find the new values depend on the chart type
  if (chart@type == "serial")
    graph_obj <- setProperties(.Object = graph_obj)
  else
    graph_obj <- setProperties(.Object = graph_obj, xField = "x", yField = name)
  
  if (type == "points")
    graph_obj <- setProperties(.Object = graph_obj, bullet = "round", maxBulletSize = 5)
  # set the type if necessary
  if (type == "smoothedLine")
    graph_obj <- setProperties(.Object = graph_obj, type = "smoothedLine")
  
  # add the graph
  addGraph(.Object = chart, amGraph = graph_obj)
}


