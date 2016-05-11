#' @title Plotting times series which aggregation
#' @description  amStockAggreg computes a stock chart.
#' 
#' @param data \code{data.frame}, data of graph.
#' @param col_date \code{character} name of date column
#' @param col_series \code{character} names of series columns
#' @param color \code{character}, color of series (in hexadecimal).
#' @param bullet \code{character}, point shape. Possible values are : "diamond", "square", 
#' "bubble",  "yError", "xError", "round", "triangleLeft", "triangleRight", "triangleUp", 
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
#' @param ZoomButton \code{data.frame}, 3 columns : 
#' Unit, times unit
#' multiple : multiple*unit 
#' label : button's label
#' @param precision \code{numeric}, digits precision

#' @examples
#' data("data_stock_2")
#' amStockAggreg(data_stock_2, "date", c("ts1", "ts2"))
#' 
#' \donttest{
#'
#' amStockAggreg(data_stock_2, "date", c("ts1", "ts2"), bullet = "round")
#' amStockAggreg(data_stock_2, "date", c("ts1", "ts2"), bullet = "round",
#'               groupToPeriods = c('hh', 'DD', '10DD'),)
#' 
#' amStockAggreg(data_stock_2, "date", c("ts1", "ts2"), bullet = "round",
#'               groupToPeriods = c('12hh', 'DD', '10DD'),
#'               maxSeries = 50)
#' 
#' 
#' 
#' amStockAggreg(data_stock_2, "date", c("ts1", "ts2"), bullet = "round",
#'               groupToPeriods = c('12hh', 'DD', '10DD'),
#'               maxSeries = 50, precision = 5)
#' 
#' amStockAggreg(data_stock_2, "date", c("ts1", "ts2"), bullet =  c("diamond", "square"),
#'              groupToPeriods = c('12hh', 'DD', '10DD'),
#'              maxSeries = 50, aggregation = "Sum")
#' 
#' 
#' ZoomButton <- data.frame(Unit = c("DD", "DD", "MAX"), multiple = c(1, 2 ,1),
#'                         label = c("Day","2 days", "MAX"))
#' amStockAggreg(data_stock_2, "date", c("ts1", "ts2"), bullet = "round",
#'              ZoomButton = ZoomButton, main = "My title", ylab = "Interest")
#'              
#' }
#' @seealso 
#' \itemize{
#' \item{\url{https://datastorm-open.github.io/introduction_ramcharts/}}
#' }
#'
#' @export
#'
amStockAggreg <- function(data, col_date,
                    col_series,
                    main = "",
                    ylab = "",
                    color = c("#2E2EFE", "#31B404", "#FF4000", "#AEB404"),
                    bullet = NULL, 
                    aggregation = "Average",
                    maxSeries = 300,
                    groupToPeriods = c('ss', 'mm', 'hh', 'DD', 'MM', 'YYYY'),
                    ZoomButton = NULL,
                    precision = 1){
  
  
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
  
  #bullet
  if(!is.null(bullet))
  {
    .testIn(bullet, c("diamond", "square", 
                      "bubble",  "yError", "xError", "round", "triangleLeft", "triangleRight", "triangleUp"))
  }
  
  #aggregation
  .testIn(vect = aggregation,control =  c("Low", "High", "Average", "Sum"))
  
  #maxSeries
  .testNumericLength1(num = maxSeries)
  
  #.testIn(vect = groupToPeriods, control = c('ss', 'mm', 'hh', 'DD', 'MM', 'YYYY'))
  
  #ZoomButton
  if(!is.null(ZoomButton))
  {
    .testIn(vect = names(ZoomButton),control =  c("Unit","multiple","label"))
    #.testIn(vect = ZoomButton$Unit,control =  c('ss', 'mm', 'hh', 'DD', 'MM', 'YYYY', 'MAX'))
    .testNumeric(num = ZoomButton$multiple)
  }
  
  #precision
  .testNumericLength1(precision)
  
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
  
  
  if(length(color) >= nrow(graph_maker))
  {
    graph_maker$color <- color[1:nrow(graph_maker)]
  }else{
    graph_maker$color <- color
  }
  
  if(length(bullet) >= nrow(graph_maker))
  {
    graph_maker$bullet <- bullet[1:nrow(graph_maker)]
  }else{
    graph_maker$bullet <- bullet
  }
  
  graph_maker$aggregation <- aggregation
  
  
  stockgraph <- apply(graph_maker,1 , function(x) {
    stockGraph(title =  x["column"][[1]],
      id = x["column"][[1]] , connect = FALSE, valueField = x["column"][[1]],
      comparable = TRUE, periodValue = x["aggregation"][[1]],
      compareField = x["column"][[1]],
      balloonText = paste0(x["column"][[1]], ' : <b>[[value]]</b>'),
      lineColor = x["color"][[1]],
      useDataSetColors = FALSE,
      bullet = ifelse(is.na(x["bullet"]), "none"[[1]], x["bullet"])[[1]],
      precision = precision
    )
  })
  
  
  
  periodZoom <- periodSelector( position = 'bottom' ,inputFieldsEnabled = FALSE)
  
  if(!is.null(ZoomButton))
  {
    for(i in 1:nrow(ZoomButton))
    {
      if(i == 1){
        

        periodZoom <- pipeR::pipeline(periodZoom,
                                      addPeriod( period = ZoomButton$Unit[i],
                                                 selected = TRUE, count = ZoomButton$multiple[i],
                                                 label =  ZoomButton$label[i])
        )
      }else{
        periodZoom <- pipeR::pipeline(periodZoom,
                                      addPeriod( period = ZoomButton$Unit[i],
                                                 count = ZoomButton$multiple[i],
                                                 label =  ZoomButton$label[i])
        )
      }
      
    }
  }

  

  
  ## Plot
  graph <- pipeR::pipeline(
    amStockChart(dataDateFormat = 'YYYY-MM-DD JJ:NN:ss', useUTC = TRUE) ,
    addDataSet(pipeR::pipeline(
      dataSet(categoryField = col_date) ,
      setDataProvider(data, keepNA = FALSE),
      
      # setDataSets(
      #   dataSet(
      #     
      #   )
      # )
      # 
      setFieldMappings(fieldMapping))),
      addPanel(panel(
        title = ylab, 
        stockGraphs = stockgraph,
        stockLegend = stockLegend(labelText = "[[title]]", useGraphSettings = TRUE)
      )),
    #addTitle(title(text = main)),
    setChartCursorSettings( valueBalloonsEnabled = TRUE, fullWidth = TRUE,
                            cursorAlpha = 0.1, valueLineBalloonEnabled = TRUE,
                            valueLineEnabled = TRUE, valueLineAlpha = 0.5,
                            categoryBalloonDateFormats = mycategoryBalloonDateFormat),
    setPeriodSelector(periodZoom, position = "bottom", inputFieldsEnabled = FALSE),
    setCategoryAxesSettings(parseDates = TRUE, minPeriod = 'fff',
                            groupToPeriods = groupToPeriods, maxSeries = maxSeries),
    setPanelsSettings(marginTop = 30),
    setLegendSettings(position = "bottom"),
    plot
  )
  graph[[1]]$chartData$panels[[1]]$titles <- list(title(text = main))
  graph
  
}