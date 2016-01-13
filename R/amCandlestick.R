#' @title Plotting candlestick chart using rAmCharts
#' @description  amCandlestick computes a candlestick chart of the given value.
#' @param data \code{data.frame} dataframe with at least 5 columns : 
#' category, open (numeric), close (numeric), low (numeric),
#' high (numeric).
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
#' 
#' @example ./examples/amCandlestick_examples.R
#' 
#' @export

amCandlestick <- function(data, xlab = "", ylab = "", horiz = FALSE, labelRotation = 45,
                          positiveColor = "#7f8da9", negativeColor = "#db4c3c",
                          names = c("low", "open", "close", "high"),
                          dataDateFormat = NULL, minPeriod = ifelse(!is.null(dataDateFormat), "DD", ""))
{
  
  if (!is.data.frame(data) | !any(c("date", "open", "close", "low", "high") %in% colnames(data))) {
    stop ("data must be a data frame which at least the columns 'date' (character),
          'open' (numeric), 'close' (numeric), 'low' (numeric) and 'high' (numeric).")
  } else {}
  if (!is.character(data$category)) {
    stop("column 'category' of the dataframe data must be character")
  } else {}
  if (!is.numeric(data$open)) {
    stop("column 'open' of the dataframe data must be numeric")
  } else {}
  if (!is.numeric(data$close)) {
    stop("column 'close' of the dataframe data must be numeric")
  } else {}
  if (!is.numeric(data$low)) {
    stop("column 'low' of the dataframe data must be numeric")
  } else {}
  if (!is.numeric(data$high)) {
    stop("column 'high' of the dataframe data must be numeric")
  } else {}
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
  
  pipeR::pipeline(
    amSerialChart(dataProvider = data, categoryField = "category", precision = 2,
                  dataDateFormat = dataDateFormat, rotate = horiz),
    addValueAxis(title = xlab, position = 'left', gridAlpha = 0.1),
    setCategoryAxis(title = ylab, labelRotation = labelRotation, axisAlpha = 0, gridAlpha = 0.1,
                    parseDates = parseDates, minPeriod = minPeriod),
    addGraph(id = "g1", openField = "open", closeField = "close", highField = "high", lowField = "low",
             valueField = "close", fillColors = positiveColor, lineColor = positiveColor, 
             negativeFillColors = negativeColor, negativeLineColor = negativeColor,
             type = "candlestick", fillAlphas = 0.8, 
             balloonText =  paste(names[4], ":<b>[[high]]</b><br>",
                                  names[3], ":<b>[[close]]</b><br>",
                                  names[2], "<b>[[open]]</b><br>",
                                  names[1], ":<b>[[low]]</b><br>"))
  )
  
}