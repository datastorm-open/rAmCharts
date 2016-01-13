#' @title Plotting candlestick chart using rAmCharts
#' @description  amCandlestick computes a candlestick chart of the given value.
#' @param data \code{data.frame} dataframe with at least 5 columns : 
#' date (character yyyy-mm-dd), open (numeric), close (numeric), low (numeric),
#' high (numeric).
#' @param period_type \code{character} either 'day', 'month' or 'year'
#' @param positive_color \code{character} color for positive values 
#' (in hexadecimal)
#' @param negative_color \code{character} color for negative values
#' (in hexadecimal)
#' @param main \code{character}, title of the graph
#' @param mainSize \code{numeric}, size of the title of the graph.
#' @param xlab \code{character} label for x-axis.
#' @param ylab \code{character} label for y-axis.
#' @param horiz \code{boolean} TRUE for an horizontal chart, FALSE for a vertical one
#' @param scrollbar \code{boolean} TRUE if you want an horizontal scrollbar to zoom
#' @examples
#' 
#' # Basic example
#' data_candle <- data.frame(date = c("2015-01-01", "2015-01-02", "2015-01-03",
#'                                    "2015-01-04", "2015-01-05", "2015-01-06",
#'                                    "2015-01-07", "2015-01-08", "2015-01-09",
#'                                    "2015-01-10", "2015-01-11", "2015-01-20"),
#'                           open = c(136.65, 135.26, 132.90, 134.94, 136.76, 131.11,
#'                                    123.12, 128.32, 128.29, 122.74, 117.01, 122.01),
#'                           close = c(136.4, 131.8, 135.2, 135.0, 134.0, 126.3, 
#'                                     125.0, 127.7, 124.0, 119.9, 117.0, 122.0),
#'                           low = c(134.15, 131.50, 128.30, 132.63, 132.00, 125.09,
#'                                   120.30, 126.50, 123.71, 119.65, 111.62, 119.82),
#'                           high = c(136.96, 135.95, 135.27, 137.24, 136.86, 133.00,
#'                                    127.75, 129.35, 128.30, 124.86, 118.50, 123.50), 
#'                           stringsAsFactors = FALSE)
#'                                    
#' amCandlestick(data = data_candle)
#' 
#' #Horizontal chart :
#' amCandlestick(data = data_candle, horiz = TRUE)
#' 
#' #With a scrollbar :
#' amCandlestick(data = data_candle, scrollbar = TRUE)
#' 
#' # datas over months
#' data_candle$date <- c("2015-01-01", "2015-02-01", "2015-03-01",
#'                       "2015-04-01", "2015-05-01", "2015-06-01",
#'                       "2015-07-01", "2015-08-01", "2015-09-01",
#'                       "2015-10-01", "2015-11-01", "2015-12-01")
#'                       
#' amCandlestick(data = data_candle, period_type = "month")
#' 
#' @export

amCandlestick <- function(data, period_type = "day", main = "", mainSize = 15, xlab = "", 
                          ylab = "", horiz = FALSE, 
                          scrollbar = FALSE, positive_color = "#7f8da9", negative_color = "#db4c3c") {
  
  if(!is.data.frame(data) | !any(c("date", "open", "close", "low", "high") %in% colnames(data))) {
    stop ("data must be a data frame which at least the columns 'date' (character),
          'open' (numeric), 'close' (numeric), 'low' (numeric) and 'high' (numeric).")
  } else {}
  
  if(!is.character(data$date)) {
    stop("column 'date' of the dataframe data must be character 'yyyy-mm-dd'")
  } else {}
  
  if(!is.numeric(data$open)) {
    stop("column 'open' of the dataframe data must be numeric")
  } else {}
  
  if(!is.numeric(data$close)) {
    stop("column 'close' of the dataframe data must be numeric")
  } else {}
  
  if(!is.numeric(data$low)) {
    stop("column 'low' of the dataframe data must be numeric")
  } else {}
  
  if(!is.numeric(data$high)) {
    stop("column 'high' of the dataframe data must be numeric")
  } else {}
  
  if(!is.character(period_type) | !(period_type %in% c("day", "month", "year"))) {
    stop("period_type must a character either 'day', 'month' or 'year'")
  } else {}
  
  period_type <- switch(period_type, "day" = "DD", "month" = "MM", "year" = "YYYY")
  
  main <- as.character(main)
  
  if(!is.numeric(mainSize)) {
    stop("mainSize must be numeric")
  } else {}
  
  if(!is.character(xlab)) {
    stop("xlab must be character.")
  } else {}
  
  if(!is.character(ylab)) {
    stop("ylab must be character.")
  } else {}
  
  if(!is.logical(horiz)) {
    stop("horiz must be logical")
  } else {}
  
  if(!is.logical(scrollbar)) {
    stop("scrollbar must be logical")
  } else {}
  
  res <- pipeR::pipeline(
    amSerialChart(dataProvider = data, categoryField = "date", dataDateFormat = "YYYY-MM-DD",
                  rotate = horiz),
    addTitle(text = main, size = mainSize),
    addValueAxis(title = ylab, position = 'left', gridAlpha = 0.1),
    setCategoryAxis(title = xlab, labelRotation = 45, axisAlpha = 0, gridAlpha = 0.1,
                    parseDates = TRUE, minPeriod = period_type),
    setChartCursor(),
    addGraph(id = "g1", openField = "open", closeField = "close", highField = "high", lowField = "low",
             valueField = "close", fillColors = positive_color, lineColor = positive_color, 
             negativeFillColors = negative_color, negativeLineColor = negative_color,
             type = "candlestick", fillAlphas = 0.8, 
             balloonText =  "Open:<b>[[open]]</b><br>Low:<b>[[low]]</b><br>High:<b>[[high]]</b><br>Close:<b>[[close]]</b><br>")
  )

  if(scrollbar) {
    res <- setChartScrollbar(res, graph = "g1", graphType = "line", scrollbarHeight = 30)
  }
  
  res
  }