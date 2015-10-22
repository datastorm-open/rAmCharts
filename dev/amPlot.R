#' @title Plot data using rAmCharts
#' 
#' @description  amPlot computes a plot of the given data values.
#' Can be a vector, a data.frame, or a formula
#' 
#' @param object generic parameter, don't use it (only for S3 definition).
#' @param x the coordinates of points in the plot.
#' @param y the y coordinates of points in the plot,
#' optional if x is an appropriate structure.
#' @param xlab a title for the x axis, default NULL
#' @param ylab a title for the y axis, default NULL
#' @param main an overall title for the plot, default ""
#' @param bullet default "round"
#' @param export default FALSE
#' @param type what type of plot should be drawn, default "p".
#' @param col default "gray"
#' @param weights for x/y charts only, default rep(1, x)
#' @param id for x/y charts only, default 1:length(x),
#' @param precision default 2
#' @param cursor default TRUE
#' @param scrollbar default TRUE
#' @param error only when type is "xError" "yError" default NULL,
#' @param xlim
#' @param ylim
#' @param cex bullet size
#' @param lty line type (dashes)
#' @param lwd line width 
#' 
#' @param ... Don't use... For S3 Definition...
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
#' @examples 
#' 
#' library(magrittr)
#' library(rAmCharts)
#' 
#' # ---
#' # x, y plot
#' # ---
#' 
#' x <- sort(rnorm(100))
#' y <- rnorm(100, sd = 10)
#' 
#' # change type
#' amPlot(x = x, y = y) # default type = "p"
#' amPlot(x = x, y = y, type = "sl")
#' amPlot(x = x, y = y, type = "l")
#' 
#' # change lty
#' amPlot(x = x, y = y, type = "l", lty = 1)
#' 
#' # add weight
#' weights <- rnorm(100, sd = 5)
#' amPlot(x = x, y = y, type = "p", weights = weights)
#' 
#' # change cex
#' amPlot(x = x, y = y, type = "p", cex = 1)
#' amPlot(x = x, y = y, type = "p", cex = 50)
#' 
#' # add title
#' amPlot(x = rnorm(100), y = rnorm(100), main = "title")
#' 
#' # ---
#' # x plot
#' # ---
#' 
#' amPlot(x = rnorm(100))
#' 
#' # add title
#' amPlot(x = rnorm(100), main = "Title")
#' 
#' # set a title
#' amPlot(x = rnorm(100), main = "Title")
#' 
#' # provide only x
#' amPlot(x = rnorm(100))
#' 
#' # change type (set to "l" by default)
#' x <- rnorm(100)
#' amPlot(x = rnorm(100, mean = 10))
#' amPlot(x = x, type = "sl")
#' amPlot(x = x, type = "st")
#' amPlot(x = x, type = "p")
#' amPlot(x = x, type = "b")
#' 
#' # change color
#' amPlot(x = rnorm(100), col = "lightblue")
#' 
#' # disable the cursor
#' amPlot(x = rnorm(100), cursor = FALSE)
#' 
#' # allow scrollbar
#' amPlot(x = rnorm(100), scrollbar = TRUE)
#' 
#' # change bullets
#' x <- rnorm(100)
#' amPlot(x = x, bullet = "diamond")
#' amPlot(x = x, bullet = "square")
#' amPlot(x = x, bullet = "triangleUp")
#' amPlot(x = x, bullet = "triangleDown")
#' amPlot(x = x, bullet = "triangleLeft")
#' amPlot(x = x, bullet = "triangleRight")
#' amPlot(x = x, bullet = "bubble")
#' amPlot(x = x, bullet = "yError", error = sample(100))
#' amPlot(x = x, bullet = "xError", error = sample(100))
#' 
#' # change lty
#' x <- rnorm(100)
#' amPlot(x = x, type = "b", lty = 1)
#' amPlot(x = x, type = "b", lty = 6)
#' 
#' # change cex
#' x <- rnorm(100)
#' amPlot(x = x, cex = 1)
#' amPlot(x = x, cex = 10)
#' 
#' # change lwd
#' x <- rnorm(100)
#' amPlot(x = x, type = "st", lwd = 1)
#' amPlot(x = x, type = "st", lwd = 2)
#' 
#' @export
#' 
amPlot.numeric <- function(x, y, bullet = "round", export = FALSE, type = "p",
                           col = "gray", id, weights = NULL, scrollbar = FALSE,
                           precision = 2, cursor = TRUE, error,
                           xlab, ylab, main, lty, cex, lwd,
                           xlim, ylim, ...)
{
  if (!requireNamespace(package = "magrittr")) {
    warning("Please install the package pipeR for running this function")
    return (NULL)
  } else {}
  
  # check arguments validity
  # ---
  bullet <- amCheck_bullet(bullet)
  if (missing(lty)) lty <- 0
  if (missing(lwd)) lwd <- 1
  
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
    
    # define width of bullet
    if (missing(cex)) cex <- 1
    
    # define the graph object depending on the type
    graph_obj <- getGraph(type = type, col = col, bullet = bullet,
                          cex = cex, lwd = lwd, lty = lty)
    
    # define axes label
    xlab <- if (missing(xlab)) "index"
    ylab <- if (missing(ylab)) deparse(substitute(x))
    
    # Add category axis at the left
    categoryAxis_obj <- if (!missing(xlim)) {
      stopifnot(is.numeric(xlim) && length(xlim) == 2)
      categoryAxis(title = xlab, position = "bottom", id = "x",
                   minimum = xlim[1], maximum = xlim[2])
    } else {
      categoryAxis(title = xlab, position = "bottom", id = "x")
    }
    
    # finally build the chart
    chart <- amSerialChart(categoryField = "cat", precision = precision) %>%
      setCategoryAxis(title = xlab, position = "bottom", id = "x") %>%
      addGraph(amGraph = graph_obj) %>%
      setCategoryAxis(categoryAxis = categoryAxis_obj) %>%
      setDataProvider(dataProvider = dt)
    
  } else {
    # the user plot an XY chart
    
    if (length(x) != length(y)) stop("'x' and 'y' lengths differ")
    
    # check (and convert) the type 
    type <- amCheck_type(type, valid = c("p", "l", "sl"))
    
    # axes label
    xlab <- if (missing(xlab)) deparse(substitute(x))
    ylab <- if (missing(ylab)) deparse(substitute(y))
    
    # width of bullets
    if (missing(cex) && missing(weights)) cex <- 0
    else if (missing(cex) && !missing(weights)) cex <- max(weights)
    
    # opacity of bullets
    bulletAlpha <- ifelse (!cex, 0, 1)
    
    # initialize dataProvider
    if (!missing(id)) {
      dt <- data.table(x = x, y = y, id = id)
      labelId  <- "Obs. <b>[[id]]</b>"
    } else {
      dt <- data.table(x = x, y = y)
      labelId  <- ""
    }
    
    if (!missing(weights)) {
      stopifnot(length(weights) == length(x))
      # max width of bullets
      if (missing(cex)) cex <- max(weights)
      # width of bullets
      weights <- round(weights, precision)
      dt <- cbind(dt, weights = weights)
      labelWeights <- "weights:<b>[[weights]]</b>"
    } else {
      labelWeights <- NULL
    }
    
    balloonText <- paste0(labelId, "<br>",
                          "x:<b>[[x]]</b> <br> y:<b>[[y]]</b><br>",
                          labelWeights, "<br>")
  
    graph_obj <- getGraphXY(type = type, col = col, bullet = bullet, cex = cex,
                            lwd = lwd, lty = lty, bulletAlpha = bulletAlpha,
                            balloonText = balloonText)

    # Add common valueAxis
    valueAxis_bottom <- if (!missing(xlim)) {
      stopifnot(is.numeric(xlim) && length(xlim) == 2)
      valueAxis(title = xlab, position = "bottom", axisAlpha = 0,
                minimum = xlim[1], maximum = xlim[2])
    } else {
      valueAxis(title = xlab, position = "bottom", axisAlpha = 0)
    }
    
    chart <- amXYChart(precision = precision) %>%
      addGraph(amGraph = graph_obj) %>%
      addValueAxis(valueAxis = valueAxis_bottom) %>%
      setDataProvider(dataProvider = dt)
    
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
#' @examples
#' 
#' library(data.table)
#' (iris <- data.table(get("iris", "package:datasets")))
#' amPlot(iris, col = "Sepal.Length")
#' 
#' amPlot(iris, col = colnames(iris)[1:4], type = c("l", "sl", "l", "st"))
#' 
#' \dontrun{
#' amPlot(iris)
#' }
#' 
#' @rdname amPlot
#' @export
#' 
amPlot.data.frame <- function(x, col, type = "l", xlab, ylab, main,
                              cursor, export, precision = 2, ...)
{
  
  ylab <- if (missing(ylab)) deparse(substitute(x))
  xlab <- if (missing(xlab)) "index"
  
  if (!missing(col)) {
    if (is.character(col)) col <- which(col %in% colnames(x))
    x <- x[, eval(col), with = FALSE]
  } else {}
  
  if (ncol(x) == 1) x <- x[[1]]
  
  if (is.data.frame(x)) {
    
    stopifnot(all(sapply(x, class) %in% "numeric"))
    
    col <- RColorBrewer::brewer.pal(11,"RdYlBu")[ncol(x)]
    names <- colnames(x)
    
    # check the type
    if (length(type) > 1 && length(type) != ncol(x))
      stop("Invalid argument type")
    
    type <- sapply(type, amCheck_type, valid = c("l", "sl", "st"))
    
    if (length(type) == 1) type <- rep(type, ncol(x))
    
    names(type) <- NULL
    
    graphs_ls <- lapply(1:ncol(x), FUN = function (i) {
      graph(balloonText = "value: <b>[[value]]</b>",
            valueField = names[i], lineAlpha = 1,
            lineColor = col[i], type = type[i])
    })
    
    x <- cbind(x, amCategory = paste("Obs.", 1:nrow(x)))
    
    chart <- pipeR::pipeline(
      amSerialChart(dataProvider = x, categoryField = "amCategory",
                    precision = precision, graphs = graphs_ls),
      setCategoryAxis(title = xlab, position = "bottom", id = "x"),
      addValueAxis(title = ylab, position = "left", id = "y")
    )
    
  } else {
    stopifnot(is.numeric(x))
    chart <- amPlot(x = x, type = type)
  }
  
  if (missing(cursor)) cursor <- TRUE
  if (missing(export)) export <- TRUE
  if (missing(main)) main <- "amPlot.data.frame"
  
  amRenderPlot(chart = chart, main = main, cursor = cursor, export = export)
}

amRenderPlot <- function (chart, main, cursor, export, scrollbar)
{
  chart %>%
    addTitle(text = main) %>%
    setChartCursor(enabled = cursor) %>%
    setChartScrollbar(enabled = scrollbar) %>%
    setExport(enabled = export)
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

getGraph <- function (type, col, bullet, cex, lwd, lty)
{
  if (type == "points" && bullet %in% c("xError", "yError"))
    graph(balloonText = "value: <b>[[value]]</b>", valueField = "x",
          lineAlpha = 0, lineColor = col, errorField = "error",
          bulletAxis = "y", bullet = bullet, bulletSize = cex)
  else if (type == "points" && !(bullet %in% c("xError", "yError")))
    graph(balloonText = "value: <b>[[value]]</b>", valueField = "x",
          lineAlpha = 0, lineColor = col,bullet = bullet, bulletSize = cex)
  else if (type == "both")
    graph(balloonText = "value: <b>[[value]]</b>", valueField = "x",
          lineAlpha = 1, lineColor = col,
          lineThickness = lwd, dashLength = lty,
          bullet = bullet, bulletSize = cex, type = "smoothedLine")
  else
    graph(balloonText = "value: <b>[[value]]</b>", valueField = "x",
          lineAlpha = 1, dashLength = lty, lineThickness = lwd,
          lineColor = col, type = type, )
}

getGraphXY <- function (type, col, bullet, cex, lwd, lty, bulletAlpha, balloonText)
{
  switch (type,
          "points" = {
            graph(balloonText = balloonText, valueField = "weights",
                  xField = "x", yField = "y", lineAlpha = 0,
                  bulletColor = col, bullet = bullet, maxBulletSize = cex)
          },
          "smoothedLine" = {
            graph(balloonText = balloonText, valueField = "weights", lineColor = col,
                  xField = "x", yField = "y", bullet = bullet, maxBulletSize = cex,
                  lineThickness = lwd, dashLength = lty, bulletAlpha = bulletAlpha)
          },
          "line" = {
            graph(balloonText = balloonText, valueField = "weights", lineColor = col,
                  xField = "x", yField = "y", bullet = bullet, maxBulletSize = cex,
                  lineThickness = lwd, dashLength = lty, bulletAlpha = bulletAlpha)
          })
}

