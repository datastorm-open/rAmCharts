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
#' @param text \code{character} text lengend
#' @param textSize \code{numeric} text size
#' @param secondAxe \code{boolean} TRUE if two axes are desired. Default is set to FALSE.
#' @param start2 \code{numeric} minimum value allowed for the second axe if secondAxe is TRUE.
#' @param end2 \code{numeric} maximum value allowed for the second axe if secondAxe is TRUE.
#' @param step2 \code{numeric} intervals size for the second axe if secondAxe is TRUE.
#' @param bands2 a list containing lists with bands informations for the second axe, named : bandStartValue (numeric),
#' bandEndValue (numeric), bandColor (hexadecimal color value)
#' @examples ./examples/amGauge_examples.R
#' 
#' @import pipeR
#' @export
#' 
amAngularGauge <- function(x, start = 0, end = 100, step = 20, 
                           bands = data.frame(start = numeric(), end = numeric(),
                                                   color = character(), width = numeric(),
                                              stringsAsFactors = FALSE), 
                          text = "", 
                          textSize = 25, secondAxe = FALSE, start2 = 0, 
                           end2 = 100, step2 = 20, 
                           bands2 = data.frame(start = numeric(), end = numeric(),
                                                    color = character(),
                                               stringsAsFactors = FALSE)) {
  ##Test
  #x
  .testNumericLength1(x)
  
  #start
  .testNumeric(start)
  
  #end
  .testNumeric(end)
  
  #step
  .testNumeric(step)
  
  #good colanmes(bands)
  .testIn("start",  colnames(bands))
  .testIn("end",  colnames(bands))
  .testIn("color",  colnames(bands))
  
  
  #good format bands
  .testNumeric(bands$start, arg = "bands$start")
  .testNumeric(bands$end, arg = "bands$end")
  .testCharacter(bands$color, arg = "bands$color")
  
  
 
  text <- as.character(text)
  
  #text size
  .testNumeric(textSize)

  .testLogicalLength1(secondAxe)

  .testNumeric(start2)
  .testNumeric(end2)
  .testNumeric(step2)

  
  #good colanmes(bands)
  .testIn("start",  colnames(bands2))
  .testIn("end",  colnames(bands2))
  .testIn("color",  colnames(bands2))
  
  
  #good format bands
  .testNumeric(bands2$start, arg = "bands2$start")
  .testNumeric(bands2$end, arg = "bands2$end")
  .testCharacter(bands2$color, arg = "bands2$color")
  
  
  .testInterval(start, bsup = end)
  .testInterval(start2, bsup = end2)
  
  .testInterval(x,binf = start, bsup = end)
  
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
    addAxis(startValue = start, endValue = end, valueInterval = step, bands = bands_1,
            bottomText = paste(x, text), bottomTextFontSize = textSize, radius = radius1)
    
  )
  if(secondAxe == TRUE) {
    res <- addAxis(res, startValue = start2, endValue = end2, valueInterval = step2,
                   bands = bands_2, inside = FALSE, gridInside = FALSE, radius = "100%")
  }
  
#   if (isTRUE(getOption('knitr.in.progress'))) {
#     return(plot(res))
#   } else {
#     return(res)
#   }
  
  res
}


#' @title Plotting solid gauge using rAmCharts
#' @description  amSolidGauge computes a gauge of the given value.
#' @param x \code{integer} an integer of the value for which the solid gauge is desired.
#' @param min \code{numeric} minimal value possible
#' @param max \code{numeric} maximal value possible
#' @param type \code{character} type of gauge : "full" or "semi"
#' @param width \code{numeric} width of the gauge
#' @param color \code{character} hexadecimal color value or a vector of colors
#' @param text \code{character} text
#' @param textSize \code{numeric} text size
#' @examples ./examples/amSolidGauge_examples.R
#' @import data.table
#' @export
#' 
amSolidGauge <- function(x, min = 0, max = 100, type = "full", width = 20,
                         color = "", text = "",
                         textSize = 20) {
  
  if (!requireNamespace(package = "pipeR")) {
    stop ("Please install the package pipeR for running this function")
  } else {}
  
  
  #x
  .testNumericLength1(x)
  
  #min
  .testNumeric(min)
  
  #max
  .testNumeric(max)

  #type
  .testCharacter(type)
  .testIn(type, c("full", "semi"))
  
  #width
  .testNumeric(width)

  #color
  .testCharacter(color)


  
  text = as.character(text)
  
  #textSize
  .testNumericLength1(textSize)

  #min, max, x
  .testInterval(min, bsup = max)
  .testInterval(x, binf = min, bsup = max)
  

  
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
    addAxis(startAngle = startAngle, endAngle = endAngle,
            startValue = min, endValue = max, bands = bands,
            bottomText = paste(x, text), bottomTextYOffset = bottomTextYOffset, bottomTextFontSize = textSize,
            tickAlpha = 0, axisColor= "#c1cdcd", labelsEnabled = FALSE)
  )
  
#   if (isTRUE(getOption('knitr.in.progress'))) {
#     return(plot(res))
#   } else {
#     return(res)
#   }
  
  res
}

