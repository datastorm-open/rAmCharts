#' @title Plotting gauge using rAmCharts
#' 
#' @description  amAngularGauge computes a gauge of the given value.
#' 
#' @param x \code{numeric}, value for which the angular gauge is desired.
#' @param start \code{numeric}, minimum value allowed.
#' @param end \code{numeric}, maximum value allowed.
#' @param step \code{numeric}, intervals size.
#' @param bands  \code{data.frame} with 4 columns : start (numeric, minimal value for the band), 
#' end (numeric, maximal value for the band), color (character, color of the band, in hexadecimal)
#' and width (numeric, width of the band). If the last column is not defined, it is auomatically
#' set to 10. 
#' @param text \code{character}, text lengend.
#' @param textSize \code{numeric}, text size.
#' @param secondAxe \code{logical}, TRUE if two axes are desired. Default is set to FALSE.
#' @param start2 \code{numeric}, minimum value allowed for the second axe if secondAxe is TRUE.
#' @param end2 \code{numeric}, maximum value allowed for the second axe if secondAxe is TRUE.
#' @param step2 \code{numeric}, intervals size for the second axe if secondAxe is TRUE.
#' @param bands2  \code{data.frame} with 4 columns : start (numeric, minimal value for the band), 
#' end (numeric, maximal value for the band), color (character, color of the band, in hexadecimal)
#' and width (numeric, width of the band). If the last column is not defined, it is auomatically
#' set to 10. 
#' @param ... see \link{amOptions} for more options.
#' 
#' @examples 
#' 
#' amAngularGauge(10)
#' 
#' @import pipeR
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
#' 
amAngularGauge <- function(x, start = 0, end = 100, step = 20, 
                           bands = data.frame(start = numeric(), end = numeric(),
                                              color = character(), width = numeric(),
                                              stringsAsFactors = FALSE), 
                           text = "", textSize = 25, secondAxe = FALSE, start2 = 0, 
                           end2 = 100, step2 = 20, 
                           bands2 = data.frame(start = numeric(), end = numeric(),
                                               color = character(),
                                               stringsAsFactors = FALSE), ...)
{
  ##Test
  #x
  .testNumericLength1(num = x)
  
  #start
  .testNumeric(num = start)
  
  #end
  .testNumeric(num = end)
  
  #step
  .testNumeric(num = step)
  
  #good colanmes(bands)
  .testIn(vect = "start",  control = colnames(bands))
  .testIn(vect = "end",  control = colnames(bands))
  .testIn(vect = "color",  control = colnames(bands))
  
  
  #good format bands
  .testNumeric(num = bands$start, arg = "bands$start")
  .testNumeric(num = bands$end, arg = "bands$end")
  .testCharacter(char = bands$color, arg = "bands$color")
  
  
  
  text <- as.character(text)
  
  #text size
  .testNumeric(num = textSize)
  
  .testLogicalLength1(logi = secondAxe)
  
  .testNumeric(num = start2)
  .testNumeric(num = end2)
  .testNumeric(num = step2)
  
  
  #good colanmes(bands)
  .testIn(vect = "start",  control = colnames(bands2))
  .testIn(vect = "end",  control = colnames(bands2))
  .testIn(vect = "color",  control = colnames(bands2))
  
  
  #good format bands
  .testNumeric(num = bands2$start, arg = "bands2$start")
  .testNumeric(num = bands2$end, arg = "bands2$end")
  .testCharacter(char = bands2$color, arg = "bands2$color")
  
  
  .testInterval(num = start, bsup = end)
  .testInterval(num = start2, bsup = end2)
  
  .testInterval(num = x, binf = start, bsup = end)
  
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
  
  chart <- pipeR::pipeline(
    amAngularGaugeChart(startValue = start, endValue = end, valueInterval = step),
    addArrow(value = x),
    addAxis(startValue = start, endValue = end, valueInterval = step, bands = bands_1,
            bottomText = paste(x, text), bottomTextFontSize = textSize, radius = radius1)
  )
  if(secondAxe == TRUE) {
    chart <- addAxis(chart, startValue = start2, endValue = end2, valueInterval = step2,
                     bands = bands_2, inside = FALSE, gridInside = FALSE, radius = "100%")
  }
  
  #   if (isTRUE(getOption('knitr.in.progress'))) {
  #     return(plot(res))
  #   } else {
  #     return(res)
  #   }
  
  amOptions(chart, ...)
}


#' @title Plotting solid gauge using rAmCharts
#' @description  amSolidGauge computes a gauge of the given value.
#' 
#' @param x \code{numeric}, value for which the angular gauge is desired.
#' @param min \code{numeric}, minimal possible value.
#' @param max \code{numeric}, maximal possible value.
#' @param type \code{character}, type of gauge : "full" or "semi".
#' @param width \code{numeric}, width of the gauge.
#' @param color \code{character}, hexadecimal color value or a vector of colors.
#' @param text \code{character}, text.
#' @param textSize \code{numeric}, text size.
#' @param ... see \code{\link{amOptions}} for more options.
#' 
#' @examples
#' amSolidGauge(x = 65)
#' 
#' \dontrun{
#' # Other examples available which can be time consuming depending on your configuration.
#' 
#' if (requireNamespace("pipeR", quietly = TRUE)) {
#' require(pipeR)
#' 
#' # Change min and max values
#' amSolidGauge(x = 65, min = 0, max = 200)
#' 
#' # Semi solid gauge
#' amSolidGauge(x = 65, type = "semi")
#' 
#' # Change width
#' amSolidGauge(x = 65, width = 50)
#' 
#' # Change color
#' amSolidGauge(x = 65, color = "#2F4F4F")
#' 
#' # Put a color scale
#' amSolidGauge(x = 10, color = c("#00ff00", "#ffd700", "#ff0000"))
#' amSolidGauge(x = 35, color = c("#00ff00", "#ffd700", "#ff0000"))
#' amSolidGauge(x = 70, color = c("#00ff00", "#ffd700", "#ff0000"))
#' amSolidGauge(x = 90, color = c("#00ff00", "#ffd700", "#ff0000"))
#' 
#' # Add some text to the printed value
#' amSolidGauge(x = 65, text = "Pct")
#' 
#' # Modify textSize value
#' amSolidGauge(x = 65, text = "Pct", textSize = 50)
#' }
#' }
#' 
#' @import data.table
#' @import pipeR
#' @import grDevices
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
#' 
amSolidGauge <- function(x, min = 0, max = 100, type = "full", width = 20,
                         color = "", text = "", textSize = 20, ...)
{
  # check arguments validity
  {
    .testNumericLength1(num = x)
    .testNumeric(num = min)
    .testNumeric(num = max)
    .testCharacter(char = type)
    .testIn(vect = type, control = c("full", "semi"))
    .testNumeric(num = width)
    .testCharacter(char = color)
    .testNumericLength1(num = textSize)
    .testInterval(num = min, bsup = max)
    .testInterval(num = x, binf = min, bsup = max)
    text <- as.character(text)
  }
  
  if (width < 100 && width > 0) {
    innerRadius <- paste0(100 - width, "%")
  } else {
    innerRadius <- "97%"
  }
  
  if (type == "full") {
    startAngle <- 0
    endAngle <- 360
    bottomTextYOffset = -110
  } else if(type == "semi") {
    startAngle <- -90
    endAngle <- 90
    bottomTextYOffset <- -150
  }
  
  col = color
  color <- tolower(color)
  if (length(color) == 1) {
    if (color == "") col <- "#1e90ff"
  } else if(length(color) > 1) {
    colfunc <- grDevices::colorRampPalette(color)
    palette <- colfunc(max - min)
    col <- palette[(x-min)]
  }
  
  bands <- list()
  bands[[1]] <- gaugeBand(startValue = 0, endValue = max, color = "#e0eeee", innerRadius = innerRadius)
  bands[[2]] <- gaugeBand(startValue = 0, endValue = x, color = col, innerRadius = innerRadius)
  
  chart <- pipeR::pipeline(
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
  
  
  amOptions(chart, ...)
}

