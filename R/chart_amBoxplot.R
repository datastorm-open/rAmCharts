#' @title Plotting boxplot using rAmCharts
#' 
#' @description  amBoxplot computes a boxplot of the given data values.
#' Can be a vector, a data.frame, or a matrix.
#' 
#' @param object a vector, data.frame, a matrix, or a formula.
#' @param data \code{data.frame}, from which the variables in formula should be taken.
#' @param use.cols \code{logical}, for matrix only. Set to TRUE to display boxplot
#' based on columns.
#' @param xlab,ylab \code{character}, labels of the axis.
#' @param ylim \code{numeric}, y values range with sensible defaults.
#' @param names \code{character}, name on x-axis, if object is a vector.
#' @param col \code{character}, color(s) to be used to fill the boxplot.
#' @param horiz \code{logical}, TRUE to rotate chart. 
#' @param id \code{character},  column name of id to 
#' identify outliers, if object is a dataframe.
#' @param ... see \link{amOptions} for more options.
#' 
#' @return An object of class \linkS4class{AmChart}.
#' 
#' @examples
#' 
#' \dontrun{
#' # 'numeric' (default)
#' amBoxplot(rnorm(100))
#' 
#' # 'formula'
#' amBoxplot(count ~ spray, data = InsectSprays)
#' 
#' # 'formula', two group
#' data <- InsectSprays
#' data$group <- c("H", "F")
#' amBoxplot(count ~ spray + group, data = data, col = c("purple", "darkblue"))
#' 
#' # 'matrix'
#' x <- matrix(nrow = 10, ncol = 5, rnorm(50))
#' amBoxplot(x)
#' 
#' # 'data.frame'
#' amBoxplot(iris[, 1:4])
#' 
#' }
#' # Other examples available which can be time consuming depending on your configuration.
#' 
#' @import data.table
#' @rdname amBoxplot
#' 
#' @seealso \link{amOptions}, \link{amBarplot}, \link{amBoxplot}, \link{amHist}, \link{amPie},
#' \link{amPlot}, \link{amTimeSeries}, \link{amStockMultiSet}, \link{amBullet}, \link{amRadar}, 
#' \link{amWind}, \link{amFunnel}, \link{amAngularGauge}, \link{amSolidGauge}, \link{amMekko},
#' \link{amCandlestick}, \link{amFloatingBar}, \link{amOHLC}, \link{amWaterfall}
#' 
#' @export
#'
#' @references See online documentation \url{https://datastorm-open.github.io/introduction_ramcharts/}
#' and \link{amChartsAPI}
amBoxplot <- function(object, ...) UseMethod("amBoxplot")


#' @rdname amBoxplot
#' @export
amBoxplot.default <- function(object, xlab = NULL, ylab = NULL, ylim = NULL,
                              names = NULL, col = "#1e90ff", horiz = FALSE, ...)
{
  
  x <- object
  value <- x
  
  if (!is.null(xlab))
    .testCharacterLength1(char = xlab)
  
  if (!is.null(ylab))
    .testCharacterLength1(char = ylab)
  
  if (is.null(names(x))) {
    id <- 1:length(x)
  } else {
    id <- names(x)
  }
  
  if (is.null(names)) {
    names <- "x"
  } else {
    names <- names[1]
  }
  
  data <- data.table(value, id, group = names)
  
  res <- data[, list(.dtBoxplotStat(list(value, id))), by = "group"]
  
  sup_options <- list(...)
  if("precision" %in% names(sup_options)){
    precision <- sup_options$precision
  } else {
    precision <- 2
  }
  final.outliers <- .finalDataBoxplot(res, col = col, precision = precision)
  
  chart <- .plotAmBoxplot(final.outliers, xlab = xlab, 
                          ylab = ylab, ylim = ylim, horiz = horiz)
  
  # return the object
  amOptions(chart, ...)
}

#' @rdname amBoxplot
#' @examples 
#' \donttest{
#' don <- data.frame(a = 1:10, b = 1:5)
#' amBoxplot(don, ylim = c(0,15))
#' }
#' @export
amBoxplot.data.frame <- function(object, id = NULL, xlab = NULL, ylab = NULL, 
                                 ylim = NULL, col = NULL, horiz = FALSE, ...)
{
  x <- object
  
  # quantitative variables
  num_variables <- sapply(x, class)
  num_variables <- names(num_variables)[!num_variables %in% c("character", "factor", "logical")]
  
  xx <- x[, num_variables[!num_variables%in%id], drop = FALSE]
  value <- do.call("c", xx)
  if (is.null(colnames(xx))) {
    group <- rep(1:ncol(x), each = nrow(xx))
  } else {
    group <- rep(colnames(xx), each = nrow(x))
  }
  if(is.null(id)){
    id <- rep(1:nrow(x), ncol(x))
  } else {
    id <- rep(x[, id], nrow(x))
  }
  
  data <- data.table(value, group, id)
  
  res <- data[, list(.dtBoxplotStat(list(value, id))), by = group]
  
  sup_options <- list(...)
  if("precision" %in% names(sup_options)){
    precision <- sup_options$precision
  } else {
    precision <- 2
  }
  final.outliers <- .finalDataBoxplot(res, col = col, precision = precision)
  
  chart <- .plotAmBoxplot(dp = final.outliers, xlab = xlab, ylab = ylab, ylim = ylim, horiz = horiz)
  
  # return the object
  chart <- setProperties(.Object = chart, RType_ = "barplot")
  amOptions(chart, ...)
}

#' @rdname amBoxplot
#' @examples 
#' \donttest{
#' # --- matrix
#' x <- matrix(nrow = 10, ncol = 5, rnorm(50))
#' 
#' amBoxplot(x) # on columns
#' colnames(x) <- LETTERS[1:5]
#' amBoxplot(x) # with names
#' amBoxplot(x, use.cols = FALSE, col = c("blue", "red"))
#' 
#' # Parameter for amOptions
#' amBoxplot(x, export = TRUE, exportFormat = "SVG")
#' }
#' @export
amBoxplot.matrix <- function(object, use.cols = TRUE, xlab = NULL, ylab = NULL, 
                             ylim = NULL, col = NULL, horiz = FALSE, ...)
{
  x <- object
  if (use.cols) {
    value <- as.vector(x)
    if (is.null(colnames(x))) {
      group <- rep(1:ncol(x), each = nrow(x))
    } else {
      group <- rep(colnames(x), each = nrow(x))
    }
    id <- rep(1:nrow(x), ncol(x))
  } else {
    value <- as.vector(t(x))
    if (is.null(rownames(x))) {
      group <- rep(1:nrow(x), each = ncol(x))
    } else {
      group <- rep(rownames(x), each = ncol(x))
    }
    id <- rep(1:ncol(x), nrow(x))
  }
  
  data <- data.table(value, group, id)
  
  res <- data[, list(.dtBoxplotStat(list(value, id))), by = group]
  
  sup_options <- list(...)
  if("precision" %in% names(sup_options)){
    precision <- sup_options$precision
  } else {
    precision <- 2
  }
  final.outliers <- .finalDataBoxplot(res, col = col, precision = precision)
  
  chart <- .plotAmBoxplot(dp = final.outliers,xlab = xlab, ylab = ylab,
                          ylim = ylim, horiz = horiz)
  
  # return the object
  amOptions(chart, ...)
}

#' @rdname amBoxplot
#' @examples 
#' \donttest{
#' # --- Formula
#' (obj <- amBoxplot(count ~ spray, data = InsectSprays))
#' 
#' # Adding parameters
#' amBoxplot(count ~ spray, data = InsectSprays, ylim = c(0,50),
#'           xlab = "spray", col = c("darkblue", "gray"))
#' 
#' # Transpose
#' amBoxplot(count ~ spray, data = InsectSprays, ylim = c(0,50), xlab = "spray", horiz = FALSE)
#' 
#' # Using a custom colum to identify outliers
#' InsectSprays$id <- paste0("ID : ", 1:nrow(InsectSprays))
#' amBoxplot(count ~ spray, data = InsectSprays, id = "id")
#' 
#' # Parameter for amOptions
#' amBoxplot(count ~ spray, data = InsectSprays, main = "amcharts")
#' }
#' @export
amBoxplot.formula <-function(object, data = NULL, id = NULL, xlab = NULL, ylab = NULL, 
                             ylim = NULL, col = NULL, horiz = FALSE, ...)
{
  
  formula <- object
  if (missing(formula) || (length(formula) != 3L))
    stop("'formula' missing or incorrect")
  
  data <- data.table(data)
  
  if (is.null(id)) {
    data[, id := 1:.N]
  } else {
    if (!id%in%colnames(data)) {
      stop("Can't find '", id, "' column in data")
    }
    setnames(data, id, "id")
  }
  
  x <- as.character(formula)[2]
  y <- gsub("^[[:space:]]|[[:space:]]$", "", 
            unlist(strsplit(as.character(formula)[3], "+", fixed = T)))
  if(length(y) > 2){
    stop("Invalid formula. Can only group per one or two variables, not more.")
  }

  res <- data[, list(.dtBoxplotStat(list(get(x), id))), keyby = y]
  # res <- res[order(res[, eval(parse(text = y))])]
  
  sup_options <- list(...)
  if("precision" %in% names(sup_options)){
    precision <- sup_options$precision
  } else {
    precision <- 2
  }
  final.outliers <- .finalDataBoxplot(res, col = col, precision = precision)
  
  chart <- .plotAmBoxplot(dp = final.outliers, xlab = xlab, 
                          ylab = ylab, ylim = ylim, horiz = horiz)
  
  # return the object
  amOptions(chart, ...)
}

.plotAmBoxplot <- function(dp, xlab = NULL, ylab = NULL, ylim = NULL, horiz = FALSE)
{

  if (!requireNamespace(package = "pipeR")) {
    warning("Please install the package pipeR for running this function")
    return (NULL)
  } else {}
  
  chart <- pipeR::pipeline(
    amSerialChart(categoryField = "cat", theme = "light", rotate = horiz),
    setDataProvider(dp, keepNA = FALSE),
    addGraph(id = "g1", type = "candlestick",
             balloonText = "High = <b>[[high_outlier]]</b><br/>3rd quart. = <b>[[close]]</b><br/>Median = <b>[[median]]</b><br/>1st quart. = <b>[[open]]</b><br/>Low = <b>[[low_outlier]]</b><br/>",
             closeField = "close", fillColorsField = "color", highField = "high",
             lineColor = "#7f8da9", lineAlpha = 1, lowField = "low",
             fillAlphas = "0.9", negativeLineColor = "#7f8da9",
             openField = "open", title = "Price:", valueField = "close",
             columnWidth = 0.4),
    addGraph(id = "g2", type = "step", valueField = "median",lineColor = "black",
             noStepRisers = TRUE, balloonText = "", lineThickness = 3, periodSpan = 0.4),
    addGraph(id = "g3", type = "step", valueField = "low_outlier",lineColor = "black",
             noStepRisers = TRUE, balloonText = "", periodSpan = 0.3),
    addGraph(id = "g4", type = "step", valueField = "high_outlier",lineColor = "black",
             noStepRisers = TRUE, balloonText = "", periodSpan = 0.3),
    addGraph(id = "g5", type = "line", valueField = "real_outlier",lineColor = "black",
             lineAlpha = 0, bullet = "round", noStepRisers = TRUE, balloonText = "", periodSpan = 0.5)
  )
  
  if (ncol(dp) > 8) {
    for (i in 1:(ncol(dp)-8)) {
      chart <- addGraph(chart, type = "line", valueField = paste0("real_outlier_", i) ,lineColor = "black",
                        lineAlpha = 0, bullet = "round", noStepRisers = TRUE, periodSpan = 0.5,
                        balloonText = paste0("[[individual_",i,"]]<br/><b> Value </b>: [[real_outlier_",i,"]]"))
    }
  }
  
  if (!is.null(ylab) & !is.null(ylim)) {
    chart <- addValueAxes(chart, title = ylab, minimum = ylim[1], maximum = ylim[length(ylim)])
  }
  
  if (!is.null(ylab) & is.null(ylim)) {
    chart <- addValueAxes(chart, title = ylab)
  }
  
  if (is.null(ylab) & !is.null(ylim)) {
    chart <- addValueAxes(chart, minimum = ylim[1], maximum = ylim[length(ylim)])
  }
  
  info_guide <- NULL
  if(all(c("cat1", "cat2") %in% colnames(dp))){
    info_guide <- dp[, list(category = cat[1], toCategory = cat[.N]),  by = "cat1"]
  }
  
  if (!is.null(xlab) & is.null(info_guide)) {
    chart <- setCategoryAxis(chart, title = xlab)
  }
  if (!is.null(info_guide)) {
    guides <- lapply(1:nrow(info_guide), function(x){
      list(category = info_guide[x, category],
           toCategory = info_guide[x, toCategory],
           lineAlpha = 0.15,
           tickLength = 30,
           expand = TRUE,
           label = info_guide[x, cat1])
    })
    
    if(is.null(xlab)){
      xlab = ""
    }
    cur_warn <- options("warn")$warn
    options(warn = -1)
    chart <- setCategoryAxis(chart, gridPosition = "start",  axisAlpha =  0.5, gridAlpha = 0, 
                             position = "left", guides = guides, 
                             labelFunction = htmlwidgets::JS("function( label, item ) {
                               return item.dataContext.cat2;
                             }"), title = xlab)
    options(warn = cur_warn)
  }
  
  # return the chart with argument 'RType_' for amOptions
  setProperties(.Object = chart, RType_ = "boxplot")
}

.dtBoxplotStat <- function (data, coef = 1.5, do.out = TRUE)
{

  xx <- data.table(x = data[[1]], id = data[[2]])[!is.na(x)]
  setkeyv(xx, "x")
  
  # n <- xx[, eval(parse(text = "sum(x)"))]
  n <- xx[, sum(x)]
  stats <- .dtFivenum(xx, na.rm = TRUE)
  
  iqr <- diff(stats[c(2, 4)])
  if (coef == 0) {
    do.out <- FALSE
  } else {
    out <- if (!is.na(iqr)) {
      xx[x < (stats[2L] - coef * iqr) | x > (stats[4L] + coef * iqr),
         list(x, id)]
      
    } else {
      data.table()
    }
    if (nrow(out) > 0) {
      stats[1] <- xx[!id %in% out[, id]][, min(x)]
      stats[5] <- xx[!id %in% out[, id]][, max(x)]
      
      # control des outliers
      if(class(out$x) == "integer"){
        out[, x := as.numeric(x)]
      }
      out <- out[, list(N = .N, id), by = "x"]
      signif_n <- 6
      in_wh <- FALSE
      while(nrow(out[, list(.N), by = "x"]) >= 500){
        out[, x := signif(x, signif_n)]
        signif_n <- signif_n -1
        in_wh <- TRUE
      }
      if(in_wh){
        out <- out[, list(N = .N, id), by = "x"]
      }
      
      out[N == 1, label := "<b> Individual </b>: "]
      out[N > 1, `:=`(label = "<b> Number of outliers </b>: ", id = N)]
      out <- unique(out[, c("x", "id", "label"), with = FALSE])
      out[, `:=`(id = paste0(label, id), label = NULL)]
      out
    }
  }
  
  list(stats = stats, out = out)
}

.dtFivenum <- function (xx, na.rm = TRUE)
{

  n <- nrow(xx)
  
  if (n == 0) {
    rep.int(NA, 5)
  } else {
    n4 <- floor((n + 3)/2)/2
    d <- c(1, n4, (n + 1)/2, n + 1 - n4, n)
    0.5 * (xx[floor(d), x] + xx[ceiling(d), x])
  }
}

.finalDataBoxplot <- function(res, col = NULL, precision = 2)
{
  if(ncol(res) == 2){

    dp <- data.table(cat = as.character(res[seq(1, nrow(res), by = 2), get(colnames(res)[1])]), 
                     round(t(data.frame(res[seq(1, nrow(res), by = 2), V1]))[, c(1,1:5, 5), drop = FALSE], precision))
    
    setnames(dp,  c("cat", "low_outlier", "low", "open", "median", "close", "high", "high_outlier"))
    
  } else {
    dp <- data.table(cat1 = as.character(res[seq(1, nrow(res), by = 2), get(colnames(res)[1])]),
                     cat2 = as.character(res[seq(1, nrow(res), by = 2), get(colnames(res)[2])]),
                     round(t(data.frame(res[seq(1, nrow(res), by = 2), V1]))[, c(1,1:5, 5), drop = FALSE], precision))
    
    setnames(dp,  c("cat1", "cat2", "low_outlier", "low", "open", "median", "close", "high", "high_outlier"))
    dp[, cat := paste(cat1, cat2, sep = "-")]  
  }

  if(is.null(col)){
    col <- "#1e90ff"
  }
  dp$color <- rep(col, length.out = nrow(dp))  # recyle colors such as c("blue", "red") in test_amBoxplot.R

  outliers <- as.list(res[seq(2, nrow(res), by = 2)])
  
  # cat <- as.character(outliers[[1]])
  cat <- dp$cat
  addcat <- sapply(1:length(outliers[[ncol(res)]]), function(x){
    if(nrow(outliers[[ncol(res)]][[x]]) > 0){
      outliers[[ncol(res)]][[x]]$cat <<- cat[x]
    } else {
      outliers[[ncol(res)]][[x]] <<- data.table(x = 1, id = 1, cat = "")[x == 0]
    }
  })
  
  outliers <- do.call("rbind", outliers[[ncol(res)]])
  
  if(nrow(outliers) > 0){
    outliers[, x := list(round(x, precision))]
    
    split.outliers <- split(outliers, outliers$cat)
    
    final.outliers <- NULL
    ctrl <- lapply(split.outliers, function(x){
      inter <- .formatOutlier(x)
      if(is.null(final.outliers)){
        final.outliers <<- inter
      }else{
        final.outliers <<- rbind(final.outliers, inter, fill = TRUE)
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

.formatOutlier <- function(data)
{

  if(is.list(data) & !"data.table" %in% class(data)){
    data <- data.frame(rev(data))
  }else{
    data <- data.frame(t(c(data[, unique(cat)], data[, x], 
                           data[, id])))
  }
  
  
  colnames(data) <- c("cat", paste0("real_outlier_", 1:((ncol(data)-1)/2)),
                      paste0("individual_", 1:((ncol(data)-1)/2)))
  
  data <- data.table(data[, c("cat",paste0(c("real_outlier_", "individual_"), rep(1:((ncol(data)-1)/2), each = 2)))])
  setkey(data, cat)
  data
}
