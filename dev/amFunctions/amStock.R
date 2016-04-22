# times <- as.POSIXct(seq(-60 * 60 *10* 24 * 50 + 1, 0, by = 3600), origin = Sys.time(), tz = 'UTC')
# times <- round(times,'hours')
# times <- data.frame(times)
# times$Mesure <- 1:length(times$times) + rep(cos(seq(-pi,pi,length.out = 100)), 12) * 500 + runif(length(times$times)) * 200
# amStock(times, "times","Mesure")
# 
# 
# data <- times
# col_date = "times"
# col_series = c("Mesure", "test")
# ZoomButton <- data.frame(unit = c("MM","DD","MAX"), multiple = c(1,4,1), label = c("1 Month", "3 Days","Max"))
# 
# 
# times$test <- runif(nrow(times))*1000
# amStock(times, "times",c("Mesure", "test"), aggregation = "Sum", ZoomButton = ZoomButton)
# bullet 
#   c("round", "square", "triangleUp", "triangleDown",
#     "triangleLeft", "triangleRight", "none")
#aggregation %in% c('Average', 'Low', 'High', 'Sum')


#' @param data \code{data.frame}, data of graph.
#' @param col_date \code{character} name of date column
#' @param col_series \code{character} names of series columns
#' @param color \code{character}, color for positive values (in hexadecimal).
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
#' period than specified in maxSeries property.
#' @param ZoomButton \code{data.frame}, 3 columns : 
#' Unit, times unit
#' multiple : multiple*unit 
#' label : button's label

amStock <- function(data, col_date,
                    col_series,
                    color = c("#2E2EFE", "#31B404", "#FF4000", "#AEB404"),
                    bullet = NULL, 
                    aggregation = "Average",
                    maxSeries = 300,
                    groupToPeriods = c('ss', 'mm', 'hh', 'DD', 'MM', 'YYYY'),
                    ZoomButton = NULL
){
  mycategoryBalloonDateFormat <- list(list(period = 'YYYY', format = 'YYYY'),
                                      list(period='MM', format = 'YYYY-MM'), 
                                      list(period = 'WW', format = 'YYYY-MM-DD'),
                                      list(period='DD', format = 'YYYY-MM-DD'), 
                                      list(period = 'hh', format = 'YYYY-MM-DD JJ:NN'),
                                      list(period='mm', format = 'YYYY-MM-DD JJ:NN'), 
                                      list(period = 'ss', format = 'YYYY-MM-DD JJ:NN:ss'),
                                      list(period='fff', format = 'YYYY-MM-DD JJ:NN:ss'))
  
  
  
  fieldMapping <- lapply(col_series, function(x) {
    list(fromField=x, toField=x)
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
    stockGraph(
      id = x["column"][[1]] , connect = FALSE, valueField = x["column"][[1]],
      comparable = TRUE, periodValue = x["aggregation"][[1]],
      compareField = x["column"][[1]],
      balloonText = paste0(x["column"][[1]], ' : <b>[[value]]</b>'),
      lineColor = x["color"][[1]],
      useDataSetColors = FALSE,
      bullet = ifelse(is.na(x["bullet"]), "none"[[1]], x["bullet"])[[1]])
  })
  
  
  
  periodZoom <- periodSelector( position = 'bottom' ,inputFieldsEnabled = FALSE)
  
  if(!is.null(ZoomButton))
  {
    for(i in 1:nrow(ZoomButton))
    {
      periodZoom <- pipeR::pipeline(periodZoom,
                                    addPeriod( period = ZoomButton$unit[i],
                                               selected = TRUE, count = ZoomButton$multiple[i],
                                               label =  ZoomButton$label[i])
      )
    }
  }
  
  
  ## Plot
  pipeR::pipeline(
    amStockChart(dataDateFormat = 'YYYY-MM-DD JJ:NN:ss') ,
    addDataSet(pipeR::pipeline(
      dataSet(title = 'first data set', categoryField = col_date) ,
      setDataProvider(data, keepNA=FALSE),
      setFieldMappings(fieldMapping))),
    addPanel(pipeR::pipeline(
      stockPanel(showCategoryAxis = TRUE, title = 'Value') ,
      setStockGraphs(
        stockgraph
      )
    )),
    setChartCursorSettings( valueBalloonsEnabled = TRUE, fullWidth = TRUE,
                            cursorAlpha = 0.1, valueLineBalloonEnabled = TRUE,
                            valueLineEnabled = TRUE, valueLineAlpha = 0.5,
                            categoryBalloonDateFormats = mycategoryBalloonDateFormat),
    setPeriodSelector(periodZoom),
    setCategoryAxesSettings(parseDates = TRUE, minPeriod = 'fff',
                            groupToPeriods = groupToPeriods, maxSeries = maxSeries),
    plot
  )
}