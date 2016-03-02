#' @title Plotting candlestick chart using rAmCharts
#' @description  amCandlestick computes a candlestick chart of the given value.
#' 
#' @param data \code{data.frame}, dataframe with at least 5 columns: 
#' category, open (numeric), close (numeric), low (numeric),
#' high (numeric). See \link{data_candleStick1} and \link{data_candleStick2}.
#' @param dataDateFormat \code{character}, default set to NULL. Even if your chart parses dates,
#' you can pass them as strings in your dataframe - 
#' all you need to do is to set data date format and the chart will parse dates to date objects.
#' Check this page for available formats.
#' Please note that two-digit years (YY) as well as literal month names (MMM)  are NOT supported in this setting.
#' @param minPeriod \code{character}, minPeriod Specifies the shortest period of your data.
#' This should be set only if dataDateFormat is not NULL.
#' Possible period values:
#' fff - milliseconds, ss - seconds, mm - minutes, hh - hours, DD - days, MM - months, YYYY - years.
#' It's also possible to supply a number for increments, i.e. '15mm'
#' which will instruct the chart that your data is supplied in 15 minute increments.
#' @param positiveColor \code{character}, color for positive values (in hexadecimal).
#' @param negativeColor \code{character}, color for negative values (in hexadecimal).
#' @param names \code{character}, names for the tooltip. Default set to c("low", "open", "close", "high").
#' @param xlab \code{character}, label for x-axis.
#' @param ylab \code{character}, label for y-axis.
#' @param horiz \code{logical}, TRUE for an horizontal chart, FALSE for a vertical one
#' @param ... see \code{\link{amOptions}} for more options.
#' 
#' @example examples/amCandlestick_examples.R
#' 
#' @export

amCandlestick <- function(data, xlab = "", ylab = "", horiz = FALSE, 
                          positiveColor = "#7f8da9", negativeColor = "#db4c3c",
                          names = c("low", "open", "close", "high"),
                          dataDateFormat = NULL,
                          minPeriod = ifelse(!is.null(dataDateFormat), "DD", ""), ...)
{
  # data format
  data$category <- as.character(data$category)
  .testIn("category", colnames(data))
  if(is.factor(data$category)) {
    data$category <- as.character(data$category)
  }
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
  
  .testCharacter(char = names)
  .testLength(param = names, len = 4)
  
  parseDates <- (!is.null(dataDateFormat))
  
  data$color <- ""
  data$color[data$open < data$close] <- positiveColor
  data$color[data$close <= data$open] <- negativeColor
  
  graph_obj <- graph(title = "negative", id = "g1", openField = "open", closeField = "close",
                     highField = "high", lowField = "low", valueField = "close", colorField = "color",
                     lineColorField = "color",
                     type = "candlestick", fillAlphas = 0.8, 
                     balloonText =  paste0(names[4], ": <b>[[high]]</b><br>",
                                           names[3], ": <b>[[close]]</b><br>",
                                           names[2], ": <b>[[open]]</b><br>",
                                           names[1], ": <b>[[low]]</b><br>"))
  
  
  chart <- pipeR::pipeline(
    amSerialChart(categoryField = "category", precision = 2,
                  dataDateFormat = dataDateFormat, rotate = horiz),
    setDataProvider(dataProvider = data, keepNA = FALSE),
    addValueAxis(title = ylab, position = 'left', gridAlpha = 0.1),
    setCategoryAxis(title = xlab, axisAlpha = 0, gridAlpha = 0.1,
                    parseDates = parseDates, minPeriod = minPeriod),
    addGraph(graph_obj)
  )
  
  # add argupment 'RType_' for amOptions
  chart <- setProperties(.Object = chart, RType_ = "candlestick")
  amOptions(chart, ...)
}