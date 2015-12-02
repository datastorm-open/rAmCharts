#' @title Plot data using rAmCharts
#' 
#' @description  amPlot computes a plot of the given data values.
#' Can be a vector, a data.frame, or a formula
#' 
#' @param x the coordinates of points in the plot numeric,
#' data.frame, formula.
#' @param y the y coordinates of points in the plot,
#' optional if x is an appropriate structure.
#' @param xlab a title for the x axis, default NULL
#' @param ylab a title for the y axis, default NULL
#' @param main an overall title for the plot, default ""
#' @param bullet default "round"
#' @param export default FALSE
#' @param type what type of plot should be drawn, default "p".
#' @param col either a factor or a \code{character} default "gray"
#' @param weights for x/y charts only, default rep(1, x)
#' @param id for x/y charts only, default 1:length(x),
#' @param precision default 2
#' @param cursor default TRUE
#' @param scrollbar default TRUE
#' @param error only when type is "xError" "yError" default NULL,
#' @param xlim range of x
#' @param ylim range of y
#' @param cex bullet size
#' @param lty line type (dashes)
#' @param lwd line width 
#' @param hideXScrollbar (\code{FALSE} by default)
#' optionnal for xy charts when scrollbar is enabled.
#' Specifies if Scrollbar of X axis (horizontal) should be hidden.
#' @param hideYScrollbar (\code{TRUE} by default)
#' optionnal for xy charts when scrollbar is enabled.
#' Specifies if Scrollbar of Y axis (vertical) should be hidden.
#' @param ... Don't use... For S3 Definition...
#' 
#' @example examples/amPlot_examples.R
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
#' @example examples/amPlot_examples.R
#' 
#' @import data.table
#' @import pipeR
#' @export
#' 
amPlot.numeric <- function(x, y, bullet = "round", type = "p", col = "gray", 
                           export = FALSE, weights = NULL, scrollbar = FALSE,
                           precision = 2, cursor = TRUE, id, error, xlab, ylab,
                           main, lty, cex, lwd, xlim, ylim, hideYScrollbar, hideXScrollbar,...)
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
    
  } else {
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
    
    # scrollbar conditions
    if (missing(hideYScrollbar)) hideYScrollbar <- TRUE
    if (missing(hideXScrollbar)) hideXScrollbar <- FALSE
    
    graph_obj <- getGraphXY(type = type, bullet = bullet, cex = cex, title = graphTitle,
                            lwd = lwd, lty = lty, bulletAlpha = bulletAlpha,
                            balloonText = balloonText, weighted = weighted)
    
    amXYChart(precision = precision) %>>%
      setProperties(hideYScrollbar = hideYScrollbar) %>>%
      setProperties(hideXScrollbar = hideXScrollbar) %>>%
      addGraph(amGraph = graph_obj) %>>%
      addValueAxis(valueAxis = valueAxis_bottom) %>>%
      setDataProvider(dataProvider = dt) %>>%
      (~ chart)
    
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
  if (missing(main)) main <- ""
  amRenderPlot(chart = chart, main = main, cursor = cursor,
               export = export, scrollbar = scrollbar)
}

#'
#' @param columns (optional) either a vector of \code{character} containing
#' the names of the series to draw, or a \code{numeric} vector of indices.
#' By default all numeric columns will be drawn.
#' @param legend enable legend ? (default \code{TRUE})
#' 
#' @import pipeR
#' @import data.table
#' 
#' @rdname amPlot
#' @export
#' 
amPlot.data.frame <- function(x, columns, type = "l", cursor = TRUE, scrollbar = FALSE,
                              export = FALSE, legend = TRUE, precision = 2, xlab, ylab, main, ...)
{
  if (missing(main)) main <- "amPlot.data.frame"
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
  
  amRenderPlot(chart = chart, main = main, cursor = cursor,
               export = export, scrollbar = scrollbar)
}

#'
#' @param data enable legend ? (default \code{TRUE})
#' 
#' @import pipeR
#' 
#' @rdname amPlot
#' @export
#' 
amPlot.formula <- function (x, data, ...)
{
  y_name <- all.vars(x[-3]) # subset variables in the lhs
  x_name <- all.vars(x[-2]) # subset variables in the rhs
  if (length(y_name) == 1) {
    chart <- amPlot(x = data[[eval(x_name)]], y = data[[eval(y_name)]],
                    xlab = eval(x_name), ylab = eval(y_name))
  } else {
    i <- 1
    chart <- amPlot(x = data[[eval(x_name)]], y = data[[eval(y_name[i])]],
                    xlab = eval(x_name), ylab = "multiple series")
    repeat {
      chart <- chart %>>%
        amLines(x = data[[eval(y_name[i])]], type = "p", title = eval(y_name[i]))
      if (i == length(y_name)) break
      else i <- i + 1
    }
  }
  chart
}

#'
#' @import pipeR
#' @noRd
#' 
amRenderPlot <- function (chart, main, cursor, export, scrollbar)
{
  addTitle(.Object = chart, text = main) %>>%
    setBalloon(borderAlpha = .5, borderThickness = 1, cornerRadius = 5) %>>%
    setChartCursor(enabled = cursor, cursorColor = "black") %>>%
    setChartScrollbar(enabled = scrollbar, oppositeAxis = FALSE, offset = 10) %>>%
    setExport(enabled = export) %>>%
    setProperties(theme = "light")
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

#' @title Add chart graph.
#'
#' @param chart \linkS4class{AmChart}.
#' @param x \code{numeric}.
#' @param type (optionnal) \code{character}.
#' @param col \code{character}, color of teh new serie.
#' @param title \code{character} used when legend is enabled.
#' 
#' @example examples/amPlot_examples.R
#' 
#' @rdname amLines
#' @export
#' 
amLines <- function(chart, x, type, col, title)
{
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
    
    if (missing(title)) title <- deparse(substitute(x))
    
    # initialize the graph object
    graph_obj <- graph(title = title, valueField = name, lineAlpha = lineAlpha, lineColor = col)
    
    # the field where to find the new values depend on the chart type
    if (chart@type == "serial")
      graph_obj <- setProperties(.Object = graph_obj, valueField = name)
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


