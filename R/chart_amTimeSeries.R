#' @title Plotting times series (with or without aggregation)
#' 
#' @description  amTimeSeries computes a stock chart.
#' 
#' @param data \code{data.frame}, data of graph.
#' @param col_date \code{character} name of date column
#' @param col_series \code{character} names of series columns
#' @param main \code{character}, title.
#' @param ylab \code{character}, value axis label.
#' @param color \code{character}, color of series (in hexadecimal).
#' @param type \code{character}, Type of graph. Possible values are : "line" (default),
#'  "column", "step", "smoothedLine"
#' @param bullet \code{character}, point shape. Possible values are : "diamond", "square", 
#' "bubble",  "yError", "xError", "round", "triangleLeft", "triangleRight", "triangleUp"
#' @param bulletSize : \code{numeric}, size of bullet.
#' @param linetype : \code{numeric}, line type, 0 : solid, number : dashed length 
#' @param linewidth : \code{numeric}, line width.
#' @param fillAlphas : \code{numeric}, fill. Between 0 (no fill) to 1.
#' @param precision \code{numeric}, default set to  1.
#' @param export \code{logical}, default set to  FALSE. TRUE to display export feature.
#' @param legend \code{logical}, enabled or not legend ? Defaut to TRUE.
#' @param legendPosition \code{character}, legend position. Possible values are :
#' "left", "right", "bottom", "top"
#' @param legendHidden \code{logical} hide some series on rendering ? Defaut to FALSE
#' @param aggregation \code{character}, aggregation type. Possible values are : 
#' "Low", "High", "Average" and "Sum"
#' @param maxSeries \code{numeric} Maximum series shown at a time.
#' In case there are more data points in the selection than maxSeries,
#' the chart will group data to longer periods,
#' for example - you have 250 days in the selection,
#' and maxSeries is 150 - the chart will group data to weeks.
#' @param groupToPeriods \code{character}, Periods to which data will
#' be grouped in case there are more data items in the selected
#' period than specified in maxSeries property. Possible value are :
#' 'ss', 'mm', 'hh', 'DD', 'MM', 'YYYY'. It's also possible to add multiple like "30mm". Or NULL to disable.
#' @param checkGroupToPeriods \code{logical}. Check groupToPeriods ? Default to TRUE then check validity
#' between data and groupToPeriods, else use directly groupToPeriods
#' @param ZoomButton \code{data.frame}, 3 or 4 columns : 
#' \itemize{
#'  \item{"Unit"}{ : Character. Times unit. 'ss', 'mm', 'hh', 'DD', 'MM', 'YYYY'}
#'  \item{"multiple"}{ : Numeric. multiple*unit }
#'  \item{"label"}{ : Character. button's label }
#'  \item{"selected"}{ : Boolean. Optional. To set initial selection. (One TRUE, others FALSE)}
#'}
#' @param ZoomButtonPosition \code{character}, zoom button position. Possible values are :
#' "left", "right", "bottom", "top"
#' @param periodFieldsSelection \code{boolean}, using zoom button, add also two fields to select period ?
#' @param scrollbar \code{boolean}, enabled or not scrollbar ? Defaut to TRUE.
#' @param scrollbarPosition \code{character}, scrollbar position. Possible values are :
#' "left", "right", "bottom", "top"
#' @param scrollbarHeight \code{numeric}, height of scroll bar. Default : 40.
#' @param scrollbarGraph \code{character}, name of serie (column) to print in scrollbar. Defaut to NULL.
#' @param cursor \code{boolean}, enabled or not cursor ? Defaut to TRUE.
#' @param cursorValueBalloonsEnabled \code{boolean}, if cursor, enabled or not balloons on cursor ? Defaut to TRUE.
#' @param creditsPosition \code{character}, credits position. Possible values are :
#' "top-right", "top-left", "bottom-right", "bottom-left"
#' @param group \code{character}, like in \code{dygraphs}, for synchronization in \code{shiny} or \code{rmarkdown}.
#' @param is_ts_module \code{boolean}. Don't use. For \link{rAmChartsTimeSeriesUI}
#' @param dataDateFormat \code{character} Data date format. Default to 'YYYY-MM-DD JJ:NN:ss'. See details.
#' @param categoryBalloonDateFormats \code{list} Date format objects for chart cursor. See details.
#' @param dateFormats \code{list} Date format objects for x-axis. See details.
#' @param ... other first level attributes
#' 
#' @examples
#' data("data_stock_2")
#' amTimeSeries(data_stock_2, "date", c("ts1", "ts2"))
#' 
#' \dontrun{
#' # upper /lower
#' data <- data_stock_2[1:50, ]
#' data$ts1low <- data$ts1-100
#' data$ts1up <- data$ts1+100
#' 
#' amTimeSeries(data, "date", list(c("ts1low", "ts1", "ts1up"), "ts2"))
#' amTimeSeries(data, "date", list(c("ts1low", "ts1", "ts1up"), "ts2"), 
#'               color = c("red", "blue"), bullet = c("round", "square"))
#' 
#' # column / step
#' amTimeSeries(data_stock_2, "date", c("ts1", "ts2"), 
#'               type = c("column", "step"), fillAlphas = c(1, 0),
#'               linewidth = c(0, 1))
#'  
#' # some paramters
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
#' amTimeSeries(data_stock_2, "date", c("ts1", "ts2"), aggregation = "Sum")
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
#' # date formats
#' 
#' amTimeSeries(data_stock_2, "date", c("ts1", "ts2"), 
#'   type = "column", fillAlphas = 1, 
#'   linewidth = 0, legendHidden = T, 
#'   categoryBalloonDateFormats = list(list(period = 'YYYY', format = 'YYYY'),
#'                                     list(period='MM', format = 'MM'), 
#'                                     list(period = 'WW', format = 'MM-DD'),
#'                                     list(period='DD', format = 'MM-DD'), 
#'                                     list(period = 'hh', format = 'MM-DD JJ:NN'),
#'                                     list(period='mm', format = 'MM-DD JJ:NN'), 
#'                                     list(period = 'ss', format = 'MM-DD JJ:NN:ss'),
#'                                     list(period='fff', format = 'MM-DD JJ:NN:ss')),
#'   dateFormats = list(list(period = 'YYYY', format = 'YYYY'),
#'                      list(period='MM', format = 'MMMM'), 
#'                      list(period = 'WW', format = 'MMMM-DD'),
#'                      list(period='DD', format = 'MMMM-DD'), 
#'                      list(period = 'hh', format = 'MM-DD JJ:NN'),
#'                      list(period='mm', format = 'MM-DD JJ:NN'), 
#'                      list(period = 'ss', format = 'MM-DD JJ:NN:ss'),
#'                      list(period='fff', format = 'MM-DD JJ:NN:ss')))              
#'              
#' }
#' 
#' @details
#' For dateFormats & categoryBalloonDateFormats
#' 
#' Available periods are: fff - millisecond, ss - second, mm - minute, hh - hour, DD - date, WW - week, MM - month, YYYY - year
#' 
#' Available formats : 
#' 
#' Year. The number of Y letters represents digits in the resulting number. I.e.:
#' YY = 05 (always two digits), YYYY = 2005
#' 
#' Month of the year. The output depends on the number of M's:
#' M = 8 (one or two digits), MM = 08 (always two digits),
#' MMM = Aug (3-letter month abbreviation), 
#' MMMM = August (full month name)
#' 
#' Week of the year W
#' 
#' Day of the month: D = 7 (one or two digits), DD = 07 (always two digits)
#' 
#' Day of week: E = 2 (single digit day of week), EE = 02 (two-digit day of week), 
#' EEE = Tue (3-letter abbreviation of the literal representation of the day of week), 
#' EEEE = Tuesday (full day of week name)
#' 
#' 	Hour: 0-23: J = 3 (one or two digits), JJ = 03 (always two digits)
#' 	
#' 	Hour: 1-24: H = 3 (one or two digits), HH = 03 (always two digits)
#' 	K 	Hour in am/pm: 0-11
#' 	L 	Hour in am/pm: 1-12 *
#' 	
#' 	Minute in hour: N = 8 (one or two digits), NN = 08 (always two digits)
#' 	
#' 	Second in minute: S = 5 (one or two digits), SS = 05 (always two digits)
#' 	
#' 	Milliseconds: QQ = 51, QQQ = 051
#' 	
#' 	Other characters 	Other characters will be displayed as they are without changing 
#' 	them. I.e.:YYYY-MM-DD = 2013-03-01
#' 	
#' @seealso \link{rAmChartsTimeSeriesUI} for shiny module, \link{amOptions}, \link{amBarplot}, \link{amBoxplot}, \link{amHist}, \link{amPie},
#' \link{amPlot}, \link{amTimeSeries}, \link{amStockMultiSet}, \link{amBullet}, \link{amRadar}, 
#' \link{amWind}, \link{amFunnel}, \link{amAngularGauge}, \link{amSolidGauge}, \link{amMekko},
#' \link{amCandlestick}, \link{amFloatingBar}, \link{amOHLC}, \link{amWaterfall}
#' 
#' @export
#'
#' @references See online documentation \url{https://datastorm-open.github.io/introduction_ramcharts/}
#' and \link{amChartsAPI}
#' 
amTimeSeries <- function(data, col_date,
                         col_series,
                         main = "",
                         ylab = "",
                         color = c("#2E2EFE", "#31B404", "#FF4000", "#AEB404"),
                         type = c("line"),
                         bullet = NULL, 
                         bulletSize = 2, 
                         linetype  = c(0, 5, 10, 15, 20),
                         linewidth = c(1, 1, 1, 1, 1, 1),
                         fillAlphas = 0,
                         precision = 1,
                         export = FALSE,
                         legend = TRUE,
                         legendPosition = "bottom",
                         legendHidden = FALSE,
                         aggregation = c("Average", "Low", "High", "Sum"),
                         maxSeries = 300,
                         groupToPeriods = c('ss', 'mm', 'hh', 'DD', 'MM', 'YYYY'),
                         checkGroupToPeriods = TRUE,
                         ZoomButton = data.frame(Unit = "MAX", multiple = 1, label ="All"),
                         ZoomButtonPosition = "bottom",
                         periodFieldsSelection = FALSE,
                         scrollbar = TRUE,
                         scrollbarPosition = "bottom",
                         scrollbarHeight = 40,
                         scrollbarGraph = NULL,
                         cursor = TRUE,
                         cursorValueBalloonsEnabled = TRUE,
                         creditsPosition = "top-right",
                         group = NULL,
                         is_ts_module = FALSE,
                         dataDateFormat = 'YYYY-MM-DD JJ:NN:SS',
                         categoryBalloonDateFormats = list(list(period = 'YYYY', format = 'YYYY'),
                                                           list(period='MM', format = 'YYYY-MM'), 
                                                           list(period = 'WW', format = 'YYYY-MM-DD'),
                                                           list(period='DD', format = 'YYYY-MM-DD'), 
                                                           list(period = 'hh', format = 'YYYY-MM-DD JJ:NN'),
                                                           list(period='mm', format = 'YYYY-MM-DD JJ:NN'), 
                                                           list(period = 'ss', format = 'YYYY-MM-DD JJ:NN:SS'),
                                                           list(period='fff', format = 'YYYY-MM-DD JJ:NN:SS')),
                         
                         dateFormats = list(list(period = 'YYYY', format = 'YYYY'),
                                                          list(period='MM', format = 'MMM'), 
                                                          list(period = 'WW', format = 'MMM DD'),
                                                          list(period='DD', format = 'MMM DD'), 
                                                          list(period = 'hh', format = 'JJ:NN'),
                                                          list(period='mm', format = 'JJ:NN'), 
                                                          list(period = 'ss', format = 'JJ:NN:SS'),
                                                          list(period='fff', format = 'JJ:NN:SS')),
                         ...)
{
  ##Test args
  
  #data
  .testFormatData(data)
  data <- data.frame(data, check.names = FALSE, stringsAsFactors = FALSE)
  
  #col_date
  .testIn(vect = col_date, control = names(data))
  
  #col_series
  init_col_series <- col_series
  if(is.list(col_series)){
    n_col_series <- sapply(col_series, length)
    col_series <- do.call("c", col_series)
    .testIn(vect = col_series, control = names(data))
    if(any(!n_col_series%in%c(3, 1))){
      stop("col_series list element must be a vector of length 1 (one curve) or 3 (upper/lower curve).")
    }
  } else if(is.vector(col_series)) {
    .testIn(vect = col_series, control = names(data))
    n_col_series <- rep(1, length(col_series))
  } else {
    stop("col_series must be a vector or a list")
  }
  
  # subset column
  data <- data[, c(col_date, col_series)]
  
  #color
  .testCharacter(char = color)
  
  # aggregation
  aggregation <- match.arg(aggregation)
  
  # Position zoom Button
  .testCharacterLength1(ZoomButtonPosition)
  .testIn(ZoomButtonPosition, c("left", "right", "bottom", "top"))
  
  # legend
  .testLogicalLength1(logi = legend)
  
  # Position legend
  .testCharacterLength1(legendPosition)
  .testIn(legendPosition, c("left", "right", "bottom", "top"))
  
  #scrollbarHeight
  .testNumericLength1(scrollbarHeight)
  
  # scrollbar
  .testLogicalLength1(logi = scrollbar)
  
  # Scroll bar position
  .testCharacterLength1(scrollbarPosition)
  .testIn(scrollbarPosition, c("left", "right", "bottom", "top"))
  
  # scroll bar graph
  if(!is.null(scrollbarGraph)){
    stopifnot(scrollbarGraph%in%colnames(data))
  }
  #creditsPosition
  .testCharacterLength1(creditsPosition)
  .testIn(creditsPosition, c("top-right", "top-left", "bottom-right", "bottom-left"))
  
  #bullet
  if (!is.null(bullet))
    .testIn(bullet, c("diamond", "square", "bubble",  "yError", "xError",
                      "round", "triangleLeft", "triangleRight", "triangleUp"))
  
  #fillAlphas
  .testNumeric(fillAlphas)
  
  #linewidth
  .testNumeric(linewidth)
  
  # maxSeries
  .testNumericLength1(num = maxSeries)
  
  #.testIn(vect = groupToPeriods, control = c('ss', 'mm', 'hh', 'DD', 'MM', 'YYYY'))
  
  #ZoomButton
  if (!is.null(ZoomButton)) {
    .testIn(vect = names(ZoomButton), control =  c("Unit", "multiple", "label", "selected"))
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
  
  data[,col_date] <- data[,col_date] + (as.POSIXlt(as.character(data[,col_date]), tz = "UTC") - data[,col_date])
  
  # groupToPeriods control
  if(nrow(data) >= 5){
    difft <- min(c(as.numeric(difftime(data[4,col_date], data[3,col_date], units = "secs")),
                   as.numeric(difftime(data[5,col_date], data[4,col_date], units = "secs"))))
  } else if(nrow(data) >= 2){
    difft <- as.numeric(difftime(data[2,col_date], data[1,col_date], units = "secs"))
  } else {
    difft <- 1
  }

  if(checkGroupToPeriods){
    if(length(groupToPeriods) == 1){
      minPeriod = groupToPeriods
      groupToPeriods <- list(groupToPeriods)
    } else {
      groupToPeriods <- controlgroupToPeriods(groupToPeriods, difft)
      minPeriod = groupToPeriods[1]
      if(length(groupToPeriods) == 1){
        groupToPeriods <- list(groupToPeriods)
      }
    }
  }
  
  # annual data
  if(isTRUE(all.equal('YYYY', groupToPeriods))){
    groupToPeriods <- c('DD', 'YYYY')
  }
  fieldMapping <- lapply(col_series, function(x) {
    list(fromField=x, toField=x, title = x)
  })
  
  graph_maker <- data.frame(column = col_series, stringsAsFactors = F)
  
  # color
  if (length(color) > 1) {
    graph_maker$color <- rep(color[1:length(n_col_series)], n_col_series)
  } else {
    graph_maker$color <- color
  }
  
  # type
  if (length(type) > 1) {
    graph_maker$type <- rep(type[1:length(n_col_series)], n_col_series)
  } else {
    graph_maker$type <- type
  }
  
  # linewidth
  if (length(linewidth) > 1) {
    graph_maker$linewidth <- rep(linewidth[1:length(n_col_series)], n_col_series)
  } else {
    graph_maker$linewidth <- linewidth
  }
  
  # bullet
  if(!is.null(bullet)){
    if (length(bullet) > 1) {
      graph_maker$bullet <- rep(bullet[1:length(n_col_series)], n_col_series)
    } else {
      graph_maker$bullet <- bullet
      graph_maker$bulletAlpha <- 1
    }
  } else {
    graph_maker$bullet <- "round"
    graph_maker$bulletSize <- 5
    graph_maker$bulletAlpha <- 0
  }
  
  # bulletSize
  if(!is.null(bullet)){
    if (length(bulletSize) > 1) {
      graph_maker$bulletSize <- rep(bulletSize[1:length(n_col_series)], n_col_series)
    } else {
      graph_maker$bulletSize <- bulletSize
    }
  }
  
  # linetype
  if (length(linetype) > 1) {
    graph_maker$dashLength <- rep(linetype[1:length(n_col_series)], n_col_series)
  } else {
    graph_maker$dashLength <- linetype
  }
  
  # fillAlphas
  if (length(fillAlphas) > 1) {
    graph_maker$fillAlphas <- rep(fillAlphas[1:length(n_col_series)], n_col_series)
  } else {
    graph_maker$fillAlphas <- fillAlphas
  }
  
  graph_maker$aggregation <- aggregation
  
  # type (curve or upper/lower)
  graph_maker$am_type <- do.call("c", lapply(n_col_series, function(x){
    if(x == 3){
      c("low", "curve-uplow", "up")
    } else {
      "curve"
    }
  }))
  
  # legend for upper / lower
  event_hide <- ""
  event_show <- ""
  if(is.list(init_col_series)){
    for(i in 1:length(init_col_series)){
      if(length(init_col_series[[i]]) == 3){
        
        event_hide <- paste0(event_hide, 
                             'if(id === "', init_col_series[[i]][2], '"){\n', 
                             'event.chart.hideGraph(event.chart.getGraphById("', init_col_series[[i]][1], '"));\n',
                             'event.chart.hideGraph(event.chart.getGraphById("', init_col_series[[i]][3], '"));}')

        event_show <- paste0(event_show, 
                             'if(id === "', init_col_series[[i]][2], '"){\n', 
                             'event.chart.showGraph(event.chart.getGraphById("', init_col_series[[i]][1], '"));\n',
                             'event.chart.showGraph(event.chart.getGraphById("', init_col_series[[i]][3], '"));}')
        
      }
    }
  }
    
  if(event_hide != ""){
    st_legend <- stockLegend(enabled = legend, labelText = "[[title]]", useGraphSettings = TRUE) %>>% addListener("hideItem", paste0('function(event) {console.info(event);var id = event.dataItem.id;\n', event_hide, '}')) %>>%
      addListener("showItem", paste0('function(event) {var id = event.dataItem.id;\n',event_show, '}'))
  } else {
    st_legend <- stockLegend(enabled = legend, labelText = "[[title]]", useGraphSettings = TRUE)
  }
  
  # hidden init
  if (length(legendHidden) > 1) {
    graph_maker$hidden <- rep(legendHidden[1:length(n_col_series)], n_col_series)
  } else {
    graph_maker$hidden <- legendHidden
  }
  
  stockgraph <- lapply(1:nrow(graph_maker), function(x) {
    if(graph_maker[x, "am_type"] == "curve"){
      stockGraph(title =  graph_maker[x, "column"],
                 id = graph_maker[x, "column"] , connect = FALSE, 
                 valueField = graph_maker[x, "column"],
                 comparable = TRUE, periodValue = graph_maker[x, "aggregation"],
                 compareField = graph_maker[x, "column"],
                 balloonText = paste0(graph_maker[x, "column"], ' : <b>[[value]]</b>'),
                 lineColor = graph_maker[x, "color"],
                 fillAlphas = graph_maker[x, "fillAlphas"],
                 bulletSize = graph_maker[x, "bulletSize"],
                 type = graph_maker[x, "type"],
                 minBulletSize = 0,
                 dashLength = graph_maker[x, "dashLength"],
                 useDataSetColors = FALSE,
                 bullet = ifelse(is.null(graph_maker[x, "bullet"]), "none", graph_maker[x, "bullet"]),
                 bulletAlpha = graph_maker[x, "bulletAlpha"],
                 precision = precision,
                 hidden = graph_maker[x, "hidden"],
                 lineThickness = graph_maker[x, "linewidth"]
      )
    } else if(graph_maker[x, "am_type"] == "low"){
      stockGraph(title =  graph_maker[x, "column"],
                 id = graph_maker[x, "column"] , connect = FALSE, 
                 valueField = graph_maker[x, "column"],
                 comparable = TRUE, periodValue = graph_maker[x, "aggregation"],
                 compareField = graph_maker[x, "column"],
                 showBalloon = FALSE,
                 lineAlpha = 0,
                 lineColor = graph_maker[x, "color"],
                 type = graph_maker[x, "type"],
                 fillAlphas = 0,
                 useDataSetColors = FALSE,
                 visibleInLegend = FALSE,
                 hidden = graph_maker[x, "hidden"],
                 precision = precision
      )
    } else if(graph_maker[x, "am_type"] == "curve-uplow"){
      stockGraph(title =  graph_maker[x, "column"],
                 id = graph_maker[x, "column"] , connect = FALSE, 
                 valueField = graph_maker[x, "column"],
                 comparable = TRUE, periodValue = graph_maker[x, "aggregation"],
                 compareField = graph_maker[x, "column"],
                 balloonText = paste0(graph_maker[x+1, "column"],' : <b> [[', graph_maker[x+1, "column"], ']] </b><br>',
                                      graph_maker[x, "column"], ' : <b> [[value]] </b><br>',
                                      graph_maker[x-1, "column"],' : <b> [[', graph_maker[x-1, "column"], ']] </b>'),
                 lineColor = graph_maker[x, "color"],
                 type = graph_maker[x, "type"],
                 fillAlphas = graph_maker[x, "fillAlphas"],
                 bulletSize = graph_maker[x, "bulletSize"],
                 minBulletSize = 0,
                 dashLength = graph_maker[x, "dashLength"],
                 useDataSetColors = FALSE,
                 bullet = ifelse(is.null(graph_maker[x, "bullet"]), "none", graph_maker[x, "bullet"]),
                 bulletAlpha = graph_maker[x, "bulletAlpha"],
                 precision = precision,
                 hidden = graph_maker[x, "hidden"],
                 lineThickness = graph_maker[x, "linewidth"]
      )
    } else if(graph_maker[x, "am_type"] == "up"){
      stockGraph(title =  graph_maker[x, "column"],
                 id = graph_maker[x, "column"] , connect = FALSE, 
                 valueField = graph_maker[x, "column"],
                 comparable = TRUE, periodValue = graph_maker[x, "aggregation"],
                 compareField = graph_maker[x, "column"],
                 type = graph_maker[x, "type"],
                 showBalloon = FALSE,
                 lineAlpha = 0,
                 lineColor = graph_maker[x, "color"],
                 fillAlphas = 0.2,
                 useDataSetColors = FALSE,
                 fillToGraph = graph_maker[x-2, "column"],
                 visibleInLegend = FALSE,
                 hidden = graph_maker[x, "hidden"],
                 precision = precision
      )
    } 
    
  })
  
  periodZoom <- periodSelector(position = ZoomButtonPosition, inputFieldsEnabled = periodFieldsSelection)
  
  if (!is.null(ZoomButton)) {
    if(!"selected" %in% colnames(ZoomButton)){
      ZoomButton$selected <- FALSE
      ZoomButton$selected[1] <- TRUE
    }
    for (i in 1:nrow(ZoomButton)) {
        periodZoom <- pipeR::pipeline(periodZoom,
                                      addPeriod(period = ZoomButton$Unit[i],
                                                selected = ZoomButton$selected[i], count = ZoomButton$multiple[i],
                                                label =  ZoomButton$label[i])
        )
    }
  }
  dataset_obj <- pipeR::pipeline(dataSet(categoryField = col_date) ,
                                 setDataProvider(data, keepNA = FALSE),
                                 setFieldMappings(fieldMapping))
  panel_obj <- pipeR::pipeline(panel(title = ylab,  stockGraphs = stockgraph),
                               setStockLegend(st_legend),
                               addTitle(text = main))
  ## Plot
  am_output <- pipeR::pipeline(
    amStockChart(dataDateFormat = dataDateFormat, useUTC = TRUE, group = group, 
                 is_ts_module = is_ts_module, ...),
    setExport(enabled = export),
    addDataSet(dataset_obj),
    addPanel(panel_obj),
    setChartCursorSettings(enabled = cursor, valueBalloonsEnabled = cursorValueBalloonsEnabled, fullWidth = TRUE,
                           cursorAlpha = 0.1, valueLineBalloonEnabled = TRUE,
                           valueLineEnabled = TRUE, valueLineAlpha = 0.5, 
                           categoryBalloonDateFormats = categoryBalloonDateFormats),
    setPeriodSelector(periodZoom),
    setCategoryAxesSettings(parseDates = TRUE, minPeriod = minPeriod,
                            groupToPeriods = groupToPeriods, 
                            maxSeries = maxSeries,
                            dateFormats = dateFormats),
    setPanelsSettings(marginTop = 30, creditsPosition = creditsPosition, thousandsSeparator = " "),
    setLegendSettings(position = legendPosition)
  )
  
  if(is.null(scrollbarGraph)){
    am_output <- setChartScrollbarSettings(am_output, enabled = scrollbar,position = scrollbarPosition, 
                                           height = scrollbarHeight)
  } else {
    am_output <- setChartScrollbarSettings(am_output, enabled = scrollbar, graph = scrollbarGraph, graphType = "line", 
                                           position = scrollbarPosition, height = scrollbarHeight)
  }

  am_output
}

controlgroupToPeriods <- function(groupToPeriods = c('30ss', 'mm', 'hh', 'DD', 'MM', 'YYYY'), 
                                  diffTime = 30){
  
  ref_period <- data.frame(periode = c('ss', 'mm', 'hh', 'DD', 'MM', 'YYYY'), 
                           seconds = c(1, 60, 3600, 24*3600, 31*24*3600, 365*24*3600))
  rownames(ref_period) <- ref_period$periode
  
  if(!is.null(groupToPeriods)){
    number <- as.numeric(gsub("ss|mm|hh|DD|MM|YYYY", "", groupToPeriods))
    number[is.na(number)] <- 1
    
    period <- gsub("^[[:digit:]]*", "", groupToPeriods)
    
    period <- ref_period[period, "seconds"]
    
    select <- groupToPeriods[period*number >= diffTime]
    
    if(length(select) == 0){
      select <- groupToPeriods[1]
    }
  } else {
    select <- c(as.character(ref_period$periode)[ref_period$seconds == diffTime])
  }
  
  #Select min period
  if(diffTime > 1){
    minperiod <- max(which(ref_period$seconds/diffTime<1))
  } else {
    minperiod <- 1
  }
  
  if(length(minperiod)>0){
    if(ref_period$seconds[minperiod+1] != diffTime){
      select <- c(paste0(diffTime/ref_period[minperiod,]$seconds,
                         ref_period[minperiod,]$periode), select)
    }
  }
  gsub("^0", "", unique(select[grepl("^[[:digit:]]*((ss)|(mm)|(hh)|(DD)|(MM)|(YYYY))$", select)]))
}