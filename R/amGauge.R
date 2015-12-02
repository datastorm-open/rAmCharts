#' @title Plotting gauge using rAmCharts
#' @description  amAngularGauge computes a gauge of the given value.
#' @param x an integer of the value for which the angular gauge is desired.
#' @param start \code{numeric} minimum value allowed
#' @param end \code{numeric} maximum value allowed
#' @param step \code{numeric} intervals size
#' @param bands a data frame with 4 columns : start (numeric, minimal value for the band), 
#' end (numeric, maximal value for the band), color (character, color of the ban, in hexadecimal)
#' and width (numeric, width of the band). If the last column is not defined, it is auomatically
#' set to 10. 
#' @param main \code{character}, title of the graph.
#' @param mainSize \code{numeric}, size of the title of the graph.
#' @param legend \code{character} text lengend
#' @param legendSize \code{numeric} text legend size
#' @param secondAxe \code{boolean} TRUE if two axes are desired. Default is set to FALSE.
#' @param start2 \code{numeric} minimum value allowed for the second axe if secondAxe is TRUE.
#' @param end2 \code{numeric} maximum value allowed for the second axe if secondAxe is TRUE.
#' @param step2 \code{numeric} intervals size for the second axe if secondAxe is TRUE.
#' @param bands2 a list containing lists with bands informations for the second axe, named : bandStartValue (numeric),
#' bandEndValue (numeric), bandColor (hexadecimal color value)
#' @examples
#' amAngularGauge(x = 25, start = 0, end = 100, step = 20, 
#'                bands = data.frame(start = c(0, 40, 60), end = c(40, 60, 100), 
#'                                   color = c("#00CC00", "#ffac29", "#ea3838"),
#'                                   stringsAsFactors = FALSE),
#'                main = "Angular gauge", mainSize = 15, legend = "km/h", legendSize = 25)
#' amAngularGauge(x = 25, start = 0, end = 100, step = 20, 
#'                bands = data.frame(start = c(0, 40, 60), end = c(40, 60, 100), 
#'                                   color = c("#00CC00", "#ffac29", "#ea3838"),
#'                                   width = c(10, 15, 20),
#'                                   stringsAsFactors = FALSE),
#'                main = "Angular gauge", mainSize = 15, legend = "km/h",
#'                secondAxe = TRUE, start2 = 100, end2 = 200, step2 = 10,
#'                bands2 = data.frame(start = c(100, 130, 170), end = c(130, 170, 200), 
#'                color = c("#00CC00", "#ffac29", "#ea3838"), stringsAsFactors = FALSE))
#' 
#' @import pipeR
#' @export
#' 
amAngularGauge <- function(x, start = 0, end = 100, step = 20, 
                           bands = data.frame(start = numeric(), end = numeric(),
                                                   color = character(), width = numeric(),
                                              stringsAsFactors = FALSE), 
                           main = "", mainSize = 15, legend = "", 
                           legendSize = 25, secondAxe = FALSE, start2 = 0, 
                           end2 = 100, step2 = 20, 
                           bands2 = data.frame(start = numeric(), end = numeric(),
                                                    color = character(),
                                               stringsAsFactors = FALSE)) {
  
  if(!is.numeric(x)) {
    stop("x must be numeric")
  } else {}
  
  if(!is.numeric(start)) {
    stop("start must be numeric")
  } else {}
  
  if(!is.numeric(end)) {
    stop("end must be numeric")
  } else {}
  
  if(!is.numeric(step)) {
    stop("step must be numeric")
  }
  
  if(!is.data.frame(bands) | !any(c("start", "end", "color") %in% colnames(bands))) {
    stop ("bands must be a data frame which at least the columns 'start' (numeric),
          'end' (numeric), and 'color' (chararcter, color in hexadecimal)")
  } else {}
  
  if(!is.numeric(bands$start)) {
    stop("column 'start' of the dataframe bands must be numeric")
  } else {}
  
  if(!is.numeric(bands$end)) {
    stop("column 'end' of the dataframe bands must be numeric")
  } else {}
  
  if(!is.character(bands$color)) {
    stop("column 'color' of the dataframe bands must be character (color in hexadecimal)")
  } else {}
  
  main <- as.character(main)
  
  if(!is.numeric(mainSize)) {
    stop("mainSize must be numeric")
  } else {}
  
  legend <- as.character(legend)
  
  if(!is.numeric(legendSize)) {
    stop("legendSize must be numeric")
  } else {}
  
  if(!is.logical(secondAxe)) {
    stop("secondAxe must be logical")
  }
  
  if(!is.numeric(start2)) {
    stop("start2 must be numeric")
  } else {}
  
  if(!is.numeric(end2)) {
    stop("end2 must be numeric")
  } else {}
  
  if(!is.numeric(step2)) {
    stop("step2 must be numeric")
  }
  
  if(!is.data.frame(bands2) | !any(c("start", "end", "color") %in% colnames(bands2))) {
    stop ("bands2 must be a data frame which at least the columns 'start' (numeric),
          'end' (numeric), and 'color' (chararcter, color in hexadecimal)")
  } else {}
  
  if(!is.numeric(bands2$start)) {
    stop("column 'start' of the dataframe bands2 must be numeric")
  } else {}
  
  if(!is.numeric(bands2$end)) {
    stop("column 'end' of the dataframe bands2 must be numeric")
  } else {}
  
  if(!is.character(bands2$color)) {
    stop("column 'color' of the dataframe bands2 must be character (color in hexadecimal)")
  } else {}
  
  if(start > end) {
    stop("start as to be inferior to end")
  } else {}
  
  if(start2 > end2) {
    stop("start2 as to be inferior to end2")
  } else {}
  
  if(x < start | x > end) {
    stop("x must be between start and end")
  }
  
  bands_1 <- list()
  
  if(nrow(bands) > 0) {
    sapply(1:nrow(bands), FUN = function(i) {
      if("width" %in% colnames(bands)) {
        if(is.numeric(bands$width)) {
          if(bands$width[i] < 100 & bands$width[i] > 0) {
            innerRadius <- paste0(100-bands$width[i], "%")
          } else {
            innerRadius <- "97%"
          }
        } else {
          innerRadius <- "97%"
        }
      } else {
        innerRadius <- "97%"
      }
      bands_1[[i]] <<- gaugeBand(startValue = bands$start[i], 
                               endValue = bands$end[i], 
                               color = bands$color[i], innerRadius = innerRadius)
    })
  } else {
    bands_1[[1]] <- gaugeBand()
  }
  
  if(secondAxe == TRUE) {
    radius1 <- "80%"
    bands_2 <- list()
    if(nrow(bands2) > 0) {
      sapply(1:nrow(bands2), FUN = function(i) {
        bands_2[[i]] <<- gaugeBand(startValue = bands2$start[i], 
                                  endValue = bands2$end[i], 
                                  color = bands2$color[i], innerRadius = "100%")
      })
    } else {
      bands_2[[1]] <- gaugeBand()
    }
  } else {
    radius1 <- "100%"
  }
  
  res <- pipeR::pipeline(
    amAngularGaugeChart(startValue = start, endValue = end, valueInterval = step),
    addArrow(value = x),
    addTitle(text = main, size = mainSize),
    addAxis(startValue = start, endValue = end, valueInterval = step, bands = bands_1,
            bottomText = paste(x, legend), bottomTextFontSize = legendSize, radius = radius1)
    
  )
  if(secondAxe == TRUE) {
    res <- addAxis(res, startValue = start2, endValue = end2, valueInterval = step2,
                   bands = bands_2, inside = FALSE, gridInside = FALSE, radius = "100%")
  }
  
  if (isTRUE(getOption('knitr.in.progress'))) {
    return(plot(res))
  } else {
    return(res)
  }
}


#' @title Plotting solid gauge using rAmCharts
#' @description  amSolidGauge computes a gauge of the given value.
#' @param x \code{integer} an integer of the value for which the solid gauge is desired.
#' @param min \code{numeric} minimal value possible
#' @param max \code{numeric} maximal value possible
#' @param main \code{character}, title of the graph.
#' @param mainSize \code{numeric}, size of the title of the graph.
#' @param type \code{character} type of gauge : "full" or "semi"
#' @param width \code{numeric} width of the gauge
#' @param color \code{character} hexadecimal color value or a vector of colors
#' @param legend \code{character} text legend
#' @param legendSize \code{numeric} text legend size
#' @examples
#' amSolidGauge(x = 75, min = 0, max = 100, type = "full", width = 20, 
#'              main = "Solid Gaude : full", color = "#1e90ff", legend = "%",
#'              legendSize = 50)
#' amSolidGauge(x = 70, min = 0, max = 100, type = "semi", width = 20, 
#'              main = "Solid Gauge : semi", color = c("#00ff00", "#ffd700", "#ff0000"), legend = "%",
#'              legendSize = 25)
#' @import data.table
#' @export
#' 
amSolidGauge <- function(x, min = 0, max = 100, type = "full", width = 20,
                           main = "", mainSize = 15, color = "", legend = "",
                         legendSize = 20) {
  
  if (!requireNamespace(package = "pipeR")) {
    stop ("Please install the package pipeR for running this function")
  } else {}
  
  if(!is.numeric(x)) {
    stop("x must be numeric")
  } else {}
  
  if(!is.numeric(min)) {
    stop("min must be numeric")
  } else {}
  
  if(!is.numeric(max)) {
    stop("max must be numeric")
  } else {}
  
  if(!is.character(type) | !type %in% c("full", "semi")) {
    stop("type must be a character, 'full' or 'semi'")
  } else {}
  
  if(!is.numeric(width)) {
    stop("width must be numeric")
  } else {}
  
  main = as.character(main)
  
  if(!is.numeric(mainSize)) {
    stop("mainSize must be numeric")
  } else {}
  
  if(!is.character(color)) {
    stop("color must be a character (color in hexadecimal)")
  } else {}
  
  legend = as.character(legend)
  
  if(!is.numeric(legendSize)) {
    stop("legendSize must be numeric")
  } else {}
  
  
  if(min > max) {
    stop("min as to be inferior to max")
  } else {}
  
  if(x < min | x > max) {
    stop("x must be between min and max")
  }
  
  if(width < 100 & width > 0) {
    innerRadius <- paste0(100-width, "%")
  } else {
    innerRadius <- "97%"
  }
  
  if(type == "full") {
    startAngle = 0
    endAngle = 360
    bottomTextYOffset = -110
  } else if(type == "semi") {
    startAngle = -90
    endAngle = 90
    bottomTextYOffset = -150
  }
  
  col = color
  color <- tolower(color)
  if(length(color)==1) {
    if(color==""){
      col <- "#1e90ff"
    } 
  } else if(length(color) > 1) {
    colfunc <- colorRampPalette(color)
    palette = colfunc(max - min)
    col <- palette[(x-min)]
  }
  
  bands <- list()
  bands[[1]] <- gaugeBand(startValue = 0, endValue = max, color = "#e0eeee", innerRadius = innerRadius)
  bands[[2]] <- gaugeBand(startValue = 0, endValue = x, color = col, innerRadius = innerRadius)
  
  res <- pipeR::pipeline(
    amAngularGaugeChart(startValue = min, endValue = max, valueInterval = max,
                        labelsEnabled = FALSE),
    addTitle(text = main, size = mainSize),
    addAxis(startAngle = startAngle, endAngle = endAngle,
            startValue = min, endValue = max, bands = bands,
            bottomText = paste(x, legend), bottomTextYOffset = bottomTextYOffset, bottomTextFontSize = legendSize,
            tickAlpha = 0, axisColor= "#c1cdcd", labelsEnabled = FALSE)
  )
  
  if (isTRUE(getOption('knitr.in.progress'))) {
    return(plot(res))
  } else {
    return(res)
  }
}

