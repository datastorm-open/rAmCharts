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

amBoxplot.formula <-function(formula, data = NULL, main = NULL, xlab = NULL, ylab = NULL, ylim = NULL, col = NULL, ..., subset, na.action = NULL){
    if(missing(formula) || (length(formula) != 3L))
      stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
      m$data <- as.data.frame(data)
    m$... <- NULL
    m$na.action <- na.action # force use of default for this method
    require(stats, quietly = TRUE)
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    value <- boxplot(split(mf[[response]], mf[-response]), plot = FALSE, ...)
    data <- data.frame(mf, individual = rownames(mf))
    plotAmBoxplot(list(value = value, data = data), main = main, xlab = xlab, ylab = ylab, ylim = ylim, col = col)
}

plotAmBoxplot <- function(result, main = NULL, xlab = NULL, ylab = NULL, ylim = NULL, col = NULL){
  
  value <- result$value
  data <- result$data
  
  dp <- data.frame(cat = value$names, round(t(value$stats)[, c(1,1:5, 5), drop = FALSE], 2))
  colnames(dp) <- c("cat", "low_outlier", "low", "open", "median", "close", "high", "high_outlier")
  
  if(length(value$out) > 0){
    outlier <- data.frame(cat = value$names[value$group[1]], real_outlier_1 = value$out[1], 
                          individual_1 = data$individual[data[,1]==value$out[1] & data[,2]==value$names[value$group[1]]])
    if(length(value$out) > 1){
      ctrl <- sapply(2:length(value$out), function(x){
        current.cat <- value$names[value$group[x]]
        new_outlier <- data.frame(cat = value$names[value$group[x]], real_outlier_1 = value$out[x],
                                  individual_1 = data$individual[data[,1]==value$out[x] & data[,2]==value$names[value$group[x]]])
        if(current.cat%in%outlier$cat){
          colnames(new_outlier)[2:3] <- paste0(c("real_outlier_", "individual_"), ncol(outlier))
        }
        outlier <<- merge(outlier, new_outlier, all = TRUE)
      })
    }
    dp <- merge(dp, outlier, all = TRUE)
  }
  
  
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
    setChartCursor()  %>>% 
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



# # formula
# res = amBoxplot(count ~ spray, data = InsectSprays)
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

