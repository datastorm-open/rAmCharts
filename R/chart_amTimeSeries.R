#' @title Plotting times series which aggregation
#' @description  amTimeSeries computes a stock chart.
#' 
#' @param data \code{data.frame}, data of graph.
#' @param col_date \code{character} name of date column
#' @param col_series \code{character} names of series columns
#' @param color \code{character}, color of series (in hexadecimal).
#' @param bullet \code{character}, point shape. Possible values are : "diamond", "square", 
#' "bubble",  "yError", "xError", "round", "triangleLeft", "triangleRight", "triangleUp"
#' @param bulletSize : \code{numeric}, size of bullet.
#' @param linetype : \code{numeric}, line type, 0 : solid, number : dashed length 
#' @param linewidth : \code{numeric}, line width.
#' @param aggregation \code{character}, aggregation type. Possible values are : 
#' "Low", "High", "Average" and "Sum"
#' @param maxSeries \code{numeric} Maximum series shown at a time.
#' In case there are more data points in the selection than maxSeries,
#' the chart will group data to longer periods,
#' for example - you have 250 days in the selection,
#' and maxSeries is 150 - the chart will group data to weeks.
#' @param groupToPeriods \code{character}, Periods to which data will
#' be gruoped in case there are more data items in the selected
#' period than specified in maxSeries property. Possible value are :
#' 'ss', 'mm', 'hh', 'DD', 'MM', 'YYYY'. It's also possible to add multiple like "30mm".
#' @param ZoomButtonPosition \code{character}, zoom button position. Possible values are :
#' "left", "right", "bottom", "top"
#' @param legendPosition \code{character}, legend position. Possible values are :
#' "left", "right", "bottom", "top"
#' @param scrollbarPosition \code{character}, scrollbar position. Possible values are :
#' "left", "right", "bottom", "top"
#' @param scrollbarHeight \code{numeric}, height of scroll bar. Default : 40.
#' @param creditsPosition \code{character}, credits position. Possible values are :
#' "top-right", "top-left", "bottom-right", "bottom-left"
#' @param ZoomButton \code{data.frame}, 3 columns : 
#' Unit, times unit
#' multiple : multiple*unit 
#' label : button's label
#' @param main \code{character}, title.
#' @param ylab \code{character}, value axis label.
#' @param precision \code{numeric}, digits precision
#' @param export \code{logical}, default set to  FALSE. TRUE to display export feature.
#' @param ... other first level attributes
#' @examples
#' data("data_stock_2")
#' amTimeSeries(data_stock_2, "date", c("ts1", "ts2"))
#' 
#' \donttest{
#' amTimeSeries(data_stock_2, "date", c("ts1", "ts2"), bullet = "round")
#' amTimeSeries(data_stock_2, "date", c("ts1", "ts2"), bullet = "round",
#'               groupToPeriods = c('hh', 'DD', '10DD'))
#' 
#' amTimeSeries(data_stock_2, "date", c("ts1", "ts2"), bullet = "round",
#'               groupToPeriods = c('12hh', 'DD', '10DD'),
#'               maxSeries = 50)
#'               
#' amTimeSeries(data_stock_2, "date", c("ts1", "ts2"), bullet = "round",
#'              groupToPeriods = c('hh', 'DD', '10DD'),
#'              linewidth = c(3, 1))
#' 
#' 
#' 
#' amTimeSeries(data_stock_2, "date", c("ts1", "ts2"), bullet = "round",
#'               groupToPeriods = c('12hh', 'DD', '10DD'),
#'               maxSeries = 50, precision = 5)
#' 
#' amTimeSeries(data_stock_2, "date", c("ts1", "ts2"), bullet =  c("diamond", "square"),
#'              linetype = 0, bulletSize = c(5, 10),
#'              groupToPeriods = c('12hh', 'DD', '10DD'),
#'              maxSeries = 50, aggregation = "Sum")
#' 
#' 
#' ZoomButton <- data.frame(Unit = c("DD", "DD", "MAX"), multiple = c(1, 2 ,1),
#'                         label = c("Day","2 days", "MAX"))
#' amTimeSeries(data_stock_2, "date", c("ts1", "ts2"), bullet = "round",
#'              ZoomButton = ZoomButton, main = "My title", ylab = "Interest")
#'              
#' amTimeSeries(data_stock_2, "date", c("ts1", "ts2"), bullet = "round",
#'              ZoomButton = ZoomButton, main = "My title", ylab = "Interest",
#'              export = TRUE, ZoomButtonPosition = "right",
#'              legendPosition = "bottom", scrollbarPosition = "top")
#'          
#' amTimeSeries(data_stock_2, "date", c("ts1", "ts2"), bullet = "round",
#'              ZoomButton = ZoomButton, main = "My title",
#'              ylab = "Interest", export = TRUE,
#'              creditsPosition = "bottom-left")
#'              
#' }
#' @seealso 
#' \itemize{
#' \item{\url{https://datastorm-open.github.io/introduction_ramcharts/}}
#' }
#'
#' @export
#'
amTimeSeries <- function(data, col_date,
                         col_series,
                         main = "",
                         ylab = "",
                         color = c("#2E2EFE", "#31B404", "#FF4000", "#AEB404"),
                         bullet = NULL, 
                         bulletSize = 2, 
                         linetype  = c(0, 5, 10, 15, 20),
                         linewidth = c(1, 1, 1, 1, 1, 1),
                         aggregation = c("Average", "Low", "High", "Sum"),
                         maxSeries = 300,
                         groupToPeriods = c('ss', 'mm', 'hh', 'DD', 'MM', 'YYYY'),
                         ZoomButton = data.frame(Unit = "MAX", multiple = 1, label ="All"),
                         ZoomButtonPosition = "bottom",
                         precision = 1,
                         export = FALSE,
                         legendPosition = "bottom",
                         scrollbarPosition = "bottom",
                         scrollbarHeight = 40,
                         creditsPosition = "top-right",
                         ...)
{
  ##Test args
  
  #data
  .testFormatData(data)
  data <- data.frame(data)
  
  #col_date
  .testIn(vect = col_date, control = names(data))
  
  #col_series
  .testIn(vect = col_series, control = names(data))
  
  #color
  .testCharacter(char = color)
  
  # aggregation
  aggregation <- match.arg(aggregation)
  
  # Position zoom Button
  .testCharacterLength1(ZoomButtonPosition)
  .testIn(ZoomButtonPosition, c("left", "right", "bottom", "top"))
  
  # Position jegend
  .testCharacterLength1(legendPosition)
  .testIn(legendPosition, c("left", "right", "bottom", "top"))
  
  #scrollbarHeight
  .testNumericLength1(scrollbarHeight)
  
  # Scroll bar position
  .testCharacterLength1(scrollbarPosition)
  .testIn(scrollbarPosition, c("left", "right", "bottom", "top"))
  
  #creditsPosition
  .testCharacterLength1(creditsPosition)
  .testIn(creditsPosition, c("top-right", "top-left", "bottom-right", "bottom-left"))
  
  

  
  #bullet
  if (!is.null(bullet))
    .testIn(bullet, c("diamond", "square", "bubble",  "yError", "xError",
                      "round", "triangleLeft", "triangleRight", "triangleUp"))
  
  #linewidth
  .testNumeric(linewidth)
  
  # maxSeries
  .testNumericLength1(num = maxSeries)
  
  #.testIn(vect = groupToPeriods, control = c('ss', 'mm', 'hh', 'DD', 'MM', 'YYYY'))
  
  #ZoomButton
  if (!is.null(ZoomButton)) {
    .testIn(vect = names(ZoomButton),control =  c("Unit","multiple","label"))
    #.testIn(vect = ZoomButton$Unit,control =  c('ss', 'mm', 'hh', 'DD', 'MM', 'YYYY', 'MAX'))
    .testNumeric(num = ZoomButton$multiple)
  }
  
  #precision
  .testNumericLength1(num = precision)
  
  # labels
  .testCharacterLength1(char = ylab)
  .testCharacterLength1(char = main)
  
  #Export
  .testLogicalLength1(logi = export)
  
  
  mycategoryBalloonDateFormat <- list(list(period = 'YYYY', format = 'YYYY'),
                                      list(period='MM', format = 'YYYY-MM'), 
                                      list(period = 'WW', format = 'YYYY-MM-DD'),
                                      list(period='DD', format = 'YYYY-MM-DD'), 
                                      list(period = 'hh', format = 'YYYY-MM-DD JJ:NN'),
                                      list(period='mm', format = 'YYYY-MM-DD JJ:NN'), 
                                      list(period = 'ss', format = 'YYYY-MM-DD JJ:NN:ss'),
                                      list(period='fff', format = 'YYYY-MM-DD JJ:NN:ss'))
  
  data$date <- data$date + (as.POSIXlt(as.character(data$date), tz = "UTC") - data$date)
  
  fieldMapping <- lapply(col_series, function(x) {
    list(fromField=x, toField=x, title = x)
  })
  
  graph_maker <- data.frame(column = col_series)
  
  if (length(color) >= nrow(graph_maker)) {
    graph_maker$color <- color[1:nrow(graph_maker)]
  } else {
    graph_maker$color <- color
  }
  
  
  if (length(linewidth) >= nrow(graph_maker)) {
    graph_maker$linewidth <- linewidth[1:nrow(graph_maker)]
  } else {
    graph_maker$linewidth <- linewidth
  }
  
  
  
  if (length(bullet) >= nrow(graph_maker)) {
    graph_maker$bullet <- bullet[1:nrow(graph_maker)]
  } else {
    graph_maker$bullet <- bullet
  }
  
  if (length(linetype) >= nrow(graph_maker)) {
    graph_maker$dashLength <- linetype[1:nrow(graph_maker)]
  } else {
    graph_maker$dashLength <- linetype
  }
  
  if (length(bulletSize) >= nrow(graph_maker)) {
    graph_maker$bulletSize <- bulletSize[1:nrow(graph_maker)]
  } else {
    graph_maker$bulletSize <- bulletSize
  }
  
  graph_maker$aggregation <- aggregation
  
  stockgraph <- apply(graph_maker, 1, function(x) {
    stockGraph(title =  x["column"][[1]],
               id = x["column"][[1]] , connect = FALSE, valueField = x["column"][[1]],
               comparable = TRUE, periodValue = x["aggregation"][[1]],
               compareField = x["column"][[1]],
               balloonText = paste0(x["column"][[1]], ' : <b>[[value]]</b>'),
               lineColor = x["color"][[1]],
               bulletSize = x["bulletSize"][[1]],
               dashLength = x["dashLength"][[1]],
               useDataSetColors = FALSE,
               bullet = ifelse(is.na(x["bullet"]), "none", x["bullet"])[[1]],
               precision = precision,
               lineThickness = x["linewidth"][[1]]
    )
  })
  
  periodZoom <- periodSelector( position = ZoomButtonPosition ,inputFieldsEnabled = FALSE)
  
  if (!is.null(ZoomButton)) {
    for (i in 1:nrow(ZoomButton)) {
      if (i == 1) {
        periodZoom <- pipeR::pipeline(periodZoom,
                                      addPeriod(period = ZoomButton$Unit[i],
                                                selected = TRUE, count = ZoomButton$multiple[i],
                                                label =  ZoomButton$label[i])
        )
      } else {
        periodZoom <- pipeR::pipeline(periodZoom,
                                      addPeriod(period = ZoomButton$Unit[i],
                                                count = ZoomButton$multiple[i],
                                                label =  ZoomButton$label[i])
        )
      }
    }
  }
  dataset_obj <- pipeR::pipeline(dataSet(categoryField = col_date) ,
                                 setDataProvider(data, keepNA = FALSE),
                                 setFieldMappings(fieldMapping))
  panel_obj <- pipeR::pipeline(panel(title = ylab,  stockGraphs = stockgraph),
                               setStockLegend(labelText = "[[title]]", useGraphSettings = TRUE),
                               addTitle(text = main))
  ## Plot
  pipeR::pipeline(
    amStockChart(dataDateFormat = 'YYYY-MM-DD JJ:NN:ss', useUTC = TRUE, ...) ,
    setExport(enabled = export),
    addDataSet(dataset_obj),
    addPanel(panel_obj),
    setChartCursorSettings(valueBalloonsEnabled = TRUE, fullWidth = TRUE,
                           cursorAlpha = 0.1, valueLineBalloonEnabled = TRUE,
                           valueLineEnabled = TRUE, valueLineAlpha = 0.5,
                           categoryBalloonDateFormats = mycategoryBalloonDateFormat),
    setPeriodSelector(periodZoom),
    setCategoryAxesSettings(parseDates = TRUE, minPeriod = 'fff',
                            groupToPeriods = groupToPeriods, maxSeries = maxSeries),
    setPanelsSettings(marginTop = 30, creditsPosition = creditsPosition),
    setChartScrollbarSettings(position = scrollbarPosition, height = scrollbarHeight),
    setLegendSettings(position = legendPosition)
  )
}