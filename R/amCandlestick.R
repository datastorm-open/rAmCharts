#' @title Plotting candlestick chart using rAmCharts
#' @description  amCandlestick computes a candlestick chart of the given value.
#' @param data \code{data.frame} dataframe with at least 5 columns : 
#' category, open (numeric), close (numeric), low (numeric),
#' high (numeric). See \code{\link{data_candleStick1}} and \code{\link{data_candleStick2}}
#' @param dataDateFormat \code{character}, default 'NULL'. Even if your chart parses dates,
#' you can pass them as strings in your data - 
#' all you need to do is to set data date format and the chart will parse dates to date objects.
#' Check this page for available formats.
#' Please note that two-digit years (YY) as well as literal month names (MMM)  are NOT supported in this setting.
#' @param minPeriod \code{character} minPeriod Specifies the shortest period of your data.
#' This should be set only if dataDateFormat is not 'NULL'.
#' Possible period values:
#' fff - milliseconds, ss - seconds, mm - minutes, hh - hours, DD - days, MM - months, YYYY - years.
#' It's also possible to supply a number for increments, i.e. '15mm'
#' which will instruct the chart that your data is supplied in 15 minute increments.
#' @param positiveColor \code{character} color for positive values. 
#' Must be in hexadecimal if you plan to export the chart.
#' @param negativeColor \code{character} color for negative values (in hexadecimal).
#' @param names \code{character}, names for the tooltip.
#' @param xlab \code{character} label for x-axis.
#' @param ylab \code{character} label for y-axis.
#' @param horiz \code{boolean} TRUE for an horizontal chart, FALSE for a vertical one
#' @param labelRotation \code{numeric} Rotation angle of a label. Only horizontal axis' values can be rotated.
#' If you set this for vertical axis, the setting will be ignored. Possible values from -90 to 90.
#' @param ... see \code{\link{amOptions}} for more options
#' 
#' @example examples/amCandlestick_examples.R
#' 
#' @export

amCandlestick <- function(data, xlab = "", ylab = "", horiz = FALSE, labelRotation = 45,
                          positiveColor = "#7f8da9", negativeColor = "#db4c3c",
                          names = c("low", "open", "close", "high"),
                          dataDateFormat = NULL, minPeriod = ifelse(!is.null(dataDateFormat), "DD", ""), ...)
{
  # data format
  data$category <- as.character(data$category)
  .testIn("category", colnames(data))
  .testCharacter(data$category, arg = "data$category")
  
  .testIn("open", colnames(data))
  .testNumeric(data$open, arg = "data$open")
  
  .testIn("close", colnames(data))
  .testNumeric(data$close, arg = "data$close")
  
  
  .testIn("low", colnames(data))
  .testNumeric(data$low, arg = "data$low")
  
  .testIn("high", colnames(data))
  .testNumeric(data$high, arg = "data$high")

  if (!is.null(dataDateFormat)) {
    .testCharacterLength1(char = dataDateFormat)
  } else {}
  
  .testCharacterLength1(char = minPeriod)
  .testCharacterLength1(char = xlab)
  .testCharacterLength1(char = ylab)
  .testLogicalLength1(logi = horiz)
  .testLength(param = labelRotation, len = 1)
  .testInterval(num = labelRotation, binf = -90, bsup = 90)
  .testCharacter(char = names)
  .testLength(param = names, len = 4)
  
  parseDates <- (!is.null(dataDateFormat))
  
  data$openpos <- data$open
  data$closepos <- data$close
  data$lowpos <- data$low
  data$highpos <- data$high
  
  clo <- data$close
  ope <- data$open
  
  data$open[which(ope<=clo)] <- NA
  data$close[which(ope<=clo)] <- NA
  data$low[which(ope<=clo)] <- NA
  data$high[which(ope<=clo)] <- NA
  
  data$openpos[which(ope>clo)] <- NA
  data$closepos[which(ope>clo)] <- NA
  data$lowpos[which(ope>clo)] <- NA
  data$highpos[which(ope>clo)] <- NA
  
  data$openpos <- round(data$openpos, 2)
  data$closepos <- round(data$closepos, 2)
  data$lowpos <- round(data$lowpos, 2)
  data$highpos <- round(data$highpos, 2)
  
  graph <- list()
  graph[[1]] <-     amGraph(title = "negative",id = "g1", openField = "open", closeField = "close", highField = "high", lowField = "low",
                            valueField = "close", fillColors = negativeColor, lineColor = negativeColor,
                            type = "candlestick", fillAlphas = 0.8, 
                            balloonText =  paste(names[4], ":<b>[[high]]</b><br>",
                                                 names[3], ":<b>[[close]]</b><br>",
                                                 names[2], "<b>[[open]]</b><br>",
                                                 names[1], ":<b>[[low]]</b><br>"))
  
  graph[[2]] <- amGraph(title = "positive",id = "g2", openField = "openpos", closeField = "closepos", highField = "highpos", lowField = "lowpos",
                        valueField = "closepos", fillColors =  positiveColor, lineColor = positiveColor, 
                        type = "candlestick", fillAlphas = 0.8, 
                        balloonText =  paste(names[4], ":<b>[[highpos]]</b><br>",
                                             names[3], ":<b>[[closepos]]</b><br>",
                                             names[2], "<b>[[openpos]]</b><br>",
                                             names[1], ":<b>[[lowpos]]</b><br>"))
  
  
  
  chart <- pipeR::pipeline(
    amSerialChart(dataProvider = data, categoryField = "category", precision = 2,
                  dataDateFormat = dataDateFormat, rotate = horiz),
    addValueAxis(title = xlab, position = 'left', gridAlpha = 0.1),
    setCategoryAxis(title = ylab, labelRotation = labelRotation, axisAlpha = 0, gridAlpha = 0.1,
                    parseDates = parseDates, minPeriod = minPeriod),
    setGraphs(graph)
  )
  
  
  
  
  
  chart <- amOptions(chart, ...)
  chart
  
}