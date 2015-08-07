amBoxplot <- function(x, ...) UseMethod("amBoxplot")

amBoxplot.default <-function(x, ..., 
                             main = NULL, xlab = NULL, ylab = NULL, ylim = NULL, col = NULL,
                             range = 1.5, width = NULL, varwidth = FALSE,
                             notch = FALSE, outline = TRUE, names, plot = TRUE,
                             border = par("fg"), log = "",
                             pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),
                             horizontal = FALSE, add = FALSE, at = NULL){
  args <- list(x, ...)
  namedargs <-
    if(!is.null(attributes(args)$names)) attributes(args)$names != ""
  else rep_len(FALSE, length(args))
  ## pars <- c(args[namedargs], pars)
  groups <- if(is.list(x)) x else args[!namedargs]
  if(0L == (n <- length(groups)))
    stop("invalid first argument")
  if(length(class(groups)))
    groups <- unclass(groups)
  if(!missing(names))
    attr(groups, "names") <- names
  else {
    if(is.null(attr(groups, "names")))
      attr(groups, "names") <- 1L:n
    names <- attr(groups, "names")
  }
  cls <- sapply(groups, function(x) class(x)[1L])
  cl <- if(all(cls == cls[1L])) cls[1L] else NULL
  for(i in 1L:n)
    groups[i] <- list(boxplot.stats(unclass(groups[[i]]), range)) # do.conf=notch)
  stats <- matrix(0, nrow = 5L, ncol = n)
  conf  <- matrix(0, nrow = 2L, ncol = n)
  ng <- out <- group <- numeric(0L)
  ct <- 1
  for(i in groups) {
    stats[,ct] <- i$stats
    conf [,ct] <- i$conf
    ng <- c(ng, i$n)
    if((lo <- length(i$out))) {
      out	  <- c(out,i$out)
      group <- c(group, rep.int(ct, lo))
    }
    ct <- ct+1
  }
  if(length(cl) && cl != "numeric") oldClass(stats) <- cl
  z <- list(stats = stats, n = ng, conf = conf, out = out, group = group,
            names = names)
  
  ctrl <- ifelse(is.null(nrow(x)), length(x), nrow(x))
  
  if(is.null(nrow(x))){
    if(is.null(names(x))){
      individual <- 1:length(x)
    }else{
      individual <- names(x)
    }
  }else{
    if(is.null(rownames(x))){
      individual <- rep(1:nrow(x), ncol(x))
    }else{
      individual <- rep(rownames(x), ncol(x))
    }
  } 
  
  data = data.frame(value = as.vector(as.matrix(x)), cat = rep(names, each = ctrl), individual = individual)
  plotAmBoxplot(list(value = z, data = data), main = main, xlab = xlab, ylab = ylab, ylim = ylim, col = col)
}

amBoxplot.matrix <- function(x, use.cols = TRUE, main = NULL, xlab = NULL, ylab = NULL, ylim = NULL, col = NULL, ...){
  ## Purpose: Boxplot for each column or row [use.cols= TRUE / FALSE] of a matrix
  ## -------------------------------------------------------------------------
  ## Arguments: x: a numeric matrix; use.cols: logical, columns (T) or rows (F)
  groups <- if(use.cols) split(x, rep.int(1L:ncol(x),
                                          rep.int(nrow(x), ncol(x))))
  else split(x, seq(nrow(x)))
  ## Make use of col/row names if present
  if (length(nam <- dimnames(x)[[1+use.cols]])) names(groups) <- nam
  z <- boxplot(groups, plot = FALSE, ...)
  
  ctrl <- ifelse(is.null(nrow(x)), length(x), nrow(x))
  
  if(is.null(nrow(x))){
    if(is.null(names(x))){
      individual <- 1:length(x)
    }else{
      individual <- names(x)
    }
  }else{
    if(is.null(rownames(x))){
      individual <- rep(1:nrow(x), ncol(x))
    }else{
      individual <- rep(rownames(x), ncol(x))
    }
  } 
  
  data = data.frame(value = as.vector(as.matrix(x)), cat = rep(names(groups), each = ctrl), individual = individual)
  print(data)
  plotAmBoxplot(list(value = z, data = data), main = main, xlab = xlab, ylab = ylab, ylim = ylim, col = col)
}

amBoxplot.formula <-function(formula, data = NULL, main = NULL, xlab = NULL, ylab = NULL, ylim = NULL, col = NULL){
  
  if(missing(formula) || (length(formula) != 3L))
    stop("'formula' missing or incorrect")
  
  if(!"data.table"%in%class(data)){
    data <- data.table(data)
  }
  
  data[, id := 1:.N]
  
  x <- as.character(formula)[2]
  y <- as.character(formula)[3]
  
  res <- data[, .(dtBoxplotStat(list(eval(parse(text = x)), id))), by = y]
  
  dp <- data.table(cat = res[seq(1, nrow(res), by = 2), eval(parse(text = colnames(res)[1]))], 
                   round(t(data.frame(res[seq(1, nrow(res), by = 2), V1]))[, c(1,1:5, 5), drop = FALSE], 2))
  
  
  setnames(dp,  c("cat", "low_outlier", "low", "open", "median", "close", "high", "high_outlier"))
  setkey(dp, cat)
  
  outliers <- as.list(res[seq(2, nrow(res), by = 2)])
  
  cat <- as.character(outliers[[1]])
  
  addcat <- sapply(1:length(outliers[[2]]), function(x){
    outliers[[2]][[x]]$cat <<- cat[x]
  })
  
  outliers <- do.call("rbind", outliers[[2]])
  outliers[, x := round(x, 2)]
  
  split.outliers <- split(outliers, outliers$cat)
  
  final.outliers <- NULL
  ctrl <- lapply(split.outliers, function(x){
    inter <- formatOutlier(x)
    if(is.null(final.outliers)){
      final.outliers <<- inter
    }else{
      final.outliers <<- rbind(final.outliers, inter, fill = TRUE)
    }
    NULL
  })
  
  final.outliers <- merge(dp, final.outliers, all = TRUE)
  
  plotAmBoxplot(final.outliers, main =  main, xlab = xlab, 
                ylab = ylab, ylim = ylim, col = col)
}

plotAmBoxplot <- function(dp, main = NULL, xlab = NULL, ylab = NULL, ylim = NULL, col = NULL){
  
  graph <- amSerialChart(categoryField = "cat", theme = "light") %>>% 
    setDataProvider(dp, keepNA = FALSE) %>>%
    addGraph(id = "g1", type = "candlestick",
             balloonText = "Low = <b>[[low_outlier]]</b><br/>1st quart. = <b>[[open]]</b><br/>Median = <b>[[median]]</b><br/>3rd quart. = <b>[[close]]</b><br/>High = <b>[[high_outlier]]</b><br/>",
             closeField = "close", fillColors = col, highField = "high",
             lineColor = "#7f8da9", lineAlpha = 1, lowField = "low",
             fillAlphas = "0.9",  negativeFillColors = col, negativeLineColor = "#7f8da9",
             openField = "open", title = "Price:", valueField = "close") %>>% 
    addGraph(id = "g2", type = "step", valueField = "median",lineColor = "black",
             noStepRisers = TRUE, balloonText = "", periodSpan = 0.80, lineThickness = 3) %>>% 
    addGraph(id = "g3", type = "step", valueField = "low_outlier",lineColor = "black",
             noStepRisers = TRUE, balloonText = "", periodSpan = 0.5) %>>% 
    addGraph(id = "g4", type = "step", valueField = "high_outlier",lineColor = "black",
             noStepRisers = TRUE, balloonText = "", periodSpan = 0.5) %>>%
    addGraph(id = "g5", type = "line", valueField = "real_outlier",lineColor = "black",
             lineAlpha = 0, bullet = "round", noStepRisers = TRUE, balloonText = "", periodSpan = 0.5) %>>%
    setChartCursor(oneBalloonOnly = TRUE)  %>>% 
    setExport(position = "top-right") 
  
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
  
  if(!is.null(xlab)){
    graph <- setCategoryAxis(graph, title = xlab)
  }
  
  plot(graph)
}





dtBoxplotStat <- function (data, coef = 1.5, do.out = TRUE) {
  
  xx <- data.table(x = data[[1]], id = data[[2]])[!is.na(x)]
  setkey(xx, x)
  
  n <- xx[,sum(x)]
  stats <- dtFivenum(xx, na.rm = TRUE)
  
  iqr <- diff(stats[c(2, 4)])
  if (coef == 0){
    do.out <- FALSE
  }else{
    out <- if (!is.na(iqr)) {
      xx[x < (stats[2L] - coef * iqr) | x > (stats[4L] + coef * 
                                               iqr), .(x, id)]
      
    }else{
      data.table()
    }
    if (nrow(out) > 0){
      stats[1] <- xx[!id%in%out[,id]][, min(x)]
      stats[5] <- xx[!id%in%out[,id]][, max(x)]
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
    0.5 * (xx[floor(d)] + xx[ceiling(d)])[,x]
  }
}

formatOutlier <- function(data){
  
  if(is.list(data) & !"data.table"%in%class(data)){
    data <- data.frame(rev(data))
  }else{
    data <- data.frame(t(c(data[, unique(cat)],data[,x],data[,id])))
  }
  
  
  colnames(data) <- c("cat", paste0("real_outlier_", 1:((ncol(data)-1)/2)),
                      paste0("individual_", 1:((ncol(data)-1)/2)))
  
  data <- data.table(data[, c("cat",paste0(c("real_outlier_", "individual_"), rep(1:((ncol(data)-1)/2), each = 2)))])
  setkey(data, cat)
  data
}

# 
# formula <- as.formula("count ~ spray")
# data =  InsectSprays
# 
# xlab = "NULL"
# ylab = "NULL"
# 
# boxplot(count ~ spray, data = InsectSprays, plot = FALSE)
# # formula
# res = amBoxplot(count ~ spray, data = InsectSprays, xlab = "re")
# res
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
