#' @title Plotting boxplot using rAmCharts
#' 
#' @description  amBoxplot computes a boxplot of the given data values.
#' Can be a vector, a data.frame, or a matrix
#' 
#' @param object a vector, data.frame, a matrix, or a formula !
#' @param data a data.frame from which the variables in formula should be taken
#' @param use.cols for matrix only. Boxplot on columns or rows ?
#' @param main \code{character}, title of the graph.
#' @param xlab,ylab \code{character}, labels of the axis.
#' @param ylim the range of y values with sensible defaults.
#' @param names in case of vector, name on x-axis
#' @param col color(s) to be used to fill the boxplot
#' @param horizontal Boolean. Rotate boxplot ?
#' @param id in case of using a data.frame, column name of id for identify outliers
#' @param ... Don't use... For S3 Definition...
#' @examples
#' 
#' # formula
#' (obj <- amBoxplot(count ~ spray, data = InsectSprays))
#' print(obj)
#' 
#' # adding parameters
#' amBoxplot(count ~ spray, data = InsectSprays, ylim = c(0,50),
#'  main = "Boxplot", xlab = "spray", col = c("darkblue", "gray"))
#'  
#' # transpose
#' amBoxplot(count ~ spray, data = InsectSprays, ylim = c(0,50),
#'  main = "Boxplot", xlab = "spray", horizontal = TRUE)
#'  
#' # using a custom colum to identify outliers
#' InsectSprays$id <- paste0("ID : ", 1:nrow(InsectSprays))
#' amBoxplot(count ~ spray, data = InsectSprays, id = "id")
#' 
#' # data.frame
#' don <- data.frame(a = 1:10, b = 1:5)
#' amBoxplot(don, ylim = c(0,15))
#' 
#' # matrix
#' x = matrix(nrow = 10, ncol = 5, rnorm(50))
#' 
#' amBoxplot(x) # on columns
#' colnames(x) <- LETTERS[1:5]
#' amBoxplot(x) # with names
#' amBoxplot(x, use.cols = FALSE, col = c("blue", "red"))
#' 
#' # vector
#' amBoxplot(rnorm(100))
#' 
#' @import data.table
#' @rdname amBoxplot
#' @export
#' 
amBoxplot <- function(object, ...) UseMethod("amBoxplot")


#' @rdname amBoxplot
#' 
#' @export
amBoxplot.default <- function(object, main = NULL, xlab = NULL, ylab = NULL, ylim = NULL,
                              names = NULL, col = NULL, horizontal = FALSE, ...){
  
  x <- object
  value <- x
  
  if(is.null(names(x))){
    id <- 1:length(x)
  }else{
    id <- names(x)
  }
  
  if(is.null(names)){
    names <- "x"
  }else{
    names <- names[1]
  }
  data <- data.table(value, id, group = names)
  
  res <- data[, list(dtBoxplotStat(list(value, id))), by = "group"]
  
  final.outliers <- finalDataBoxplot(res, col = col)
  
  plotAmBoxplot(final.outliers, main =  main, xlab = xlab, 
                ylab = ylab, ylim = ylim, horizontal = horizontal)
}

#' @rdname amBoxplot
#' @export
amBoxplot.data.frame <- function(object, main = NULL, id = NULL, xlab = NULL, ylab = NULL, 
                                 ylim = NULL, col = NULL, horizontal = FALSE, ...){
  
  x <- object
  xx <- x[, colnames(x)[!colnames(x)%in%id], drop = FALSE]
  value <- do.call("c", xx)
  if(is.null(colnames(xx))){
    group <- rep(1:ncol(x), each = nrow(xx))
  }else{
    group <- rep(colnames(xx), each = nrow(x))
  }
  if(is.null(id)){
    id <- rep(1:nrow(x), ncol(x))
  }else{
    id <- rep(x[, id], nrow(x))
  }
  
  data <- data.table(value, group, id)
  
  res <- data[, list(dtBoxplotStat(list(value, id))), by = group]
  
  final.outliers <- finalDataBoxplot(res, col = col)
  
  plotAmBoxplot(final.outliers, main =  main, xlab = xlab, 
                ylab = ylab, ylim = ylim, horizontal = horizontal)
}

#' @rdname amBoxplot
#' @export
amBoxplot.matrix <- function(object, use.cols = TRUE, main = NULL, xlab = NULL, ylab = NULL, 
                             ylim = NULL, col = NULL, horizontal = FALSE, ...){
  x <- object
  if(use.cols){
    value <- as.vector(x)
    if(is.null(colnames(x))){
      group <- rep(1:ncol(x), each = nrow(x))
    }else{
      group <- rep(colnames(x), each = nrow(x))
    }
    id <- rep(1:nrow(x), ncol(x))
  }else{
    value <- as.vector(t(x))
    if(is.null(rownames(x))){
      group <- rep(1:nrow(x), each = ncol(x))
    }else{
      group <- rep(rownames(x), each = ncol(x))
    }
    id <- rep(1:ncol(x), nrow(x))
  }
  
  data <- data.table(value, group, id)
  
  
  res <- data[, list(dtBoxplotStat(list(value, id))), by = group]
  
  final.outliers <- finalDataBoxplot(res, col = col)
  
  plotAmBoxplot(final.outliers, main =  main, xlab = xlab, 
                ylab = ylab, ylim = ylim, horizontal = horizontal)
  
}

#' @rdname amBoxplot
#' @export
amBoxplot.formula <-function(object, data = NULL, id = NULL, main = NULL, xlab = NULL, ylab = NULL, 
                             ylim = NULL, col = NULL, horizontal = FALSE, ...){
  
  formula <- object
  if(missing(formula) || (length(formula) != 3L))
    stop("'formula' missing or incorrect")
  
  data <- data.table(data)
  
  if(is.null(id)){
    data[, id := 1:.N]
  }else{
    if(!id%in%colnames(data)){
      stop("Can't find '", id, "' column in data")
    }
    setnames(data, id, "id")
  }
  
  x <- as.character(formula)[2]
  y <- as.character(formula)[3]
  
  res <- data[, list(dtBoxplotStat(list(eval(parse(text = x)), id))), by = y]
  
  final.outliers <- finalDataBoxplot(res, col = col)
  
  plotAmBoxplot(final.outliers, main =  main, xlab = xlab, 
                ylab = ylab, ylim = ylim, horizontal = horizontal)
}

plotAmBoxplot <- function(dp, main = NULL, xlab = NULL, ylab = NULL, ylim = NULL, horizontal = FALSE){
  
  if (!requireNamespace(package = "pipeR")) {
    stop ("Please install the package pipeR for running this function")
  } else {}
  
  graph <- pipeR::pipeline(
    amSerialChart(categoryField = "cat", theme = "light", rotate = horizontal),
    setDataProvider(dp, keepNA = FALSE),
    addGraph(id = "g1", type = "candlestick",
             balloonText = "Low = <b>[[low_outlier]]</b><br/>1st quart. = <b>[[open]]</b><br/>Median = <b>[[median]]</b><br/>3rd quart. = <b>[[close]]</b><br/>High = <b>[[high_outlier]]</b><br/>",
             closeField = "close", fillColorsField = "color", highField = "high",
             lineColor = "#7f8da9", lineAlpha = 1, lowField = "low",
             fillAlphas = "0.9", negativeLineColor = "#7f8da9",
             openField = "open", title = "Price:", valueField = "close"),
    addGraph(id = "g2", type = "step", valueField = "median",lineColor = "black",
             noStepRisers = TRUE, balloonText = "", periodSpan = 0.80, lineThickness = 3),
    addGraph(id = "g3", type = "step", valueField = "low_outlier",lineColor = "black",
             noStepRisers = TRUE, balloonText = "", periodSpan = 0.5),
    addGraph(id = "g4", type = "step", valueField = "high_outlier",lineColor = "black",
             noStepRisers = TRUE, balloonText = "", periodSpan = 0.5),
    addGraph(id = "g5", type = "line", valueField = "real_outlier",lineColor = "black",
             lineAlpha = 0, bullet = "round", noStepRisers = TRUE, balloonText = "", periodSpan = 0.5),
    setChartCursor(oneBalloonOnly = TRUE)
  )
  
  if(ncol(dp) > 8){
    for(i in 1:(ncol(dp)-8)){
      graph <- addGraph(graph, type = "line", valueField = paste0("real_outlier_", i) ,lineColor = "black",
                        lineAlpha = 0, bullet = "round", noStepRisers = TRUE, periodSpan = 0.5,
                        balloonText = paste0("<b> Individual </b>: [[individual_",i,"]]<br/><b> Value </b>: [[real_outlier_",i,"]]"))
    }
  }
  
  if(!is.null(main)){
    graph <- addTitle(graph, text = main, size = 18)
  }
  
  if(!is.null(ylab) & !is.null(ylim)){
    graph <- addValueAxes(graph, title = ylab, minimum = ylim[1], maximum = ylim[length(ylim)])
  }
  
  if(!is.null(ylab) & is.null(ylim)){
    graph <- addValueAxes(graph, title = ylab)
  }
  
  if(is.null(ylab) & !is.null(ylim)){
    graph <- addValueAxes(graph, minimum = ylim[1], maximum = ylim[length(ylim)])
  }
  
  if(!is.null(xlab)){
    graph <- setCategoryAxis(graph, title = xlab)
  }
  
  graph
}

dtBoxplotStat <- function (data, coef = 1.5, do.out = TRUE) {
  xx <- data.table(x = data[[1]], id = data[[2]])[eval(parse(text = "!is.na(x)"))]
  setkeyv(xx, "x")
  
  n <- xx[, eval(parse(text = "sum(x)"))]
  stats <- dtFivenum(xx, na.rm = TRUE)
  
  iqr <- diff(stats[c(2, 4)])
  if (coef == 0){
    do.out <- FALSE
  }else{
    out <- if (!is.na(iqr)) {
      xx[eval(parse(text = "x < (stats[2L] - coef * iqr) | x > (stats[4L] + coef * 
                                               iqr)")), eval(parse(text = "list(x, id)"))]
      
    }else{
      data.table()
    }
    if (nrow(out) > 0){
      stats[1] <- xx[eval(parse(text = "!id%in%out[, id]"))][, eval(parse(text = "min(x)"))]
      stats[5] <- xx[eval(parse(text = "!id%in%out[, id]"))][, eval(parse(text = "max(x)"))]
    }
  }
  
  list(stats = stats, out = out)
}

dtFivenum <- function (xx, na.rm = TRUE) {
  
  n <- nrow(xx)
  
  if (n == 0){
    rep.int(NA, 5)
  }else {
    n4 <- floor((n + 3)/2)/2
    d <- c(1, n4, (n + 1)/2, n + 1 - n4, n)
    0.5 * (xx[floor(d), eval(parse(text = "x"))] + xx[ceiling(d), eval(parse(text = "x"))])
  }
}

finalDataBoxplot <- function(res, col = NULL){
  
  dp <- data.table(cat = as.character(res[seq(1, nrow(res), by = 2), eval(parse(text = colnames(res)[1]))]), 
                   round(t(data.frame(res[seq(1, nrow(res), by = 2), eval(parse(text = "V1"))]))[, c(1,1:5, 5), drop = FALSE], 2))
  
  
  setnames(dp,  c("cat", "low_outlier", "low", "open", "median", "close", "high", "high_outlier"))
  
  if(is.null(col)){
    col <- "gray"
  }
  dp$color <- col
  
  outliers <- as.list(res[seq(2, nrow(res), by = 2)])
  
  cat <- as.character(outliers[[1]])
  
  addcat <- sapply(1:length(outliers[[2]]), function(x){
    outliers[[2]][[x]]$cat <<- cat[x]
  })
  
  outliers <- do.call("rbind", outliers[[2]])
  
  if(nrow(outliers) > 0){
    outliers[, eval(parse(text = "x := list(round(x, 2))"))]
    
    split.outliers <- split(outliers, outliers$cat)
    
    final.outliers <- NULL
    ctrl <- lapply(split.outliers, function(x){
      inter <- formatOutlier(x)
      if(is.null(final.outliers)){
        final.outliers <<- inter
      }else{
        final.outliers <<- rbind.data.frame(final.outliers, inter, fill = TRUE)
      }
      NULL
    })
    final.outliers$cat <- as.character(final.outliers$cat)
    final.outliers <- merge(dp, final.outliers, all = TRUE, sort = FALSE, by = "cat")
  }else{
    final.outliers <- dp
  }
  final.outliers
}

formatOutlier <- function(data){
  
  if(is.list(data) & !"data.table"%in%class(data)){
    data <- data.frame(rev(data))
  }else{
    data <- data.frame(t(c(data[, unique(cat)],data[, eval(parse(text = "x"))],data[, eval(parse(text = "id"))])))
  }
  
  
  colnames(data) <- c("cat", paste0("real_outlier_", 1:((ncol(data)-1)/2)),
                      paste0("individual_", 1:((ncol(data)-1)/2)))
  
  data <- data.table(data[, c("cat",paste0(c("real_outlier_", "individual_"), rep(1:((ncol(data)-1)/2), each = 2)))])
  setkey(data, cat)
  data
}


# # vector
# set.seed(144)
# amBoxplot(c(rnorm(20), 50),names = "A", col = "red")
# amBoxplot(rnorm(10),names = "A", ylab = "y-axis", xlab = "x-axis", main = "boxplot", ylim = c(-5,5))
# 
# # data.frame
# don <- data.frame(a = c(rnorm(10),5), b = c(4,rnorm(10)))
# boxplot(don)
# amBoxplot(don)
# 
# # matrix
# m =  matrix(ncol = 2, nrow = 6, c(5,rnorm(10),5))
# amBoxplot(m)

# nb = 100000
# data <- data.table(a = c(rnorm(nb),5), b = c(4,rnorm(nb)), c = LETTERS[1:5])
# formula <-as.formula("a ~ c")
# 
# boxplot(a ~ c, data = data, xlab = "re")
# amBoxplot(a ~ c, data = data, xlab = "re")
# x = matrix(nrow = 10, ncol = 5, rnorm(50))
# colnames(x) <- LETTERS[1:5]
# main = NULL
# xlab = NULL
# ylab = NULL
# ylim = NULL
# col = NULL
# use.cols = TRUE
# amBoxplot(x, use.cols = TRUE, main = "re", xlab = "re")
# use.cols = TRUE
# 
# formula <- as.formula("count ~ spray")
# data =  InsectSprays
# id = NULL
# main = NULL
# xlab = NULL
# ylab = NULL
# ylim = NULL
# col = NULL