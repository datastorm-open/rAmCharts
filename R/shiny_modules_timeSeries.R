#' Shiny module to render large time-series data with live server-client aggregation
#'
#' 
#' @param id  character, used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#' @param input   standard, \code{shiny} input
#' @param output  standard, \code{shiny} output
#' @param session standard, \code{shiny} session
#' @param data : data.frame to transform.
#' @param col_date Date column name, default to "date".
#'                   Must be "POSIXct"
#' @param col_series Column name of quantitative variable(s) to be
#'                     transformed. Default to setdiff(colnames(data), "date") 
#' @param maxPoints : Maximal number of rows in results
#' @param ts All enabled aggregation. Default to c("5 min",  "10 min", "30 min", "hour", "3 hour", "12 hour", "day", "week", "month", "year").
#'             Can be a number, in seconds, or a character string
#'             containing one of "min", "hour", "day"....
#'             This can optionally be preceded by a positive integer
#'             and a space
#' @param tz : Timezone of result. Defaut to "UTC".
#' @param fun_aggr : Aggregation function to use ("min", "max", "sum", "mean", "first", "last").
#'                   Default to "mean".
#' @param treat_missing : Boolean. Default to FALSE
#'                        Whether or not to interpolate missing values ?
#'                        see \code{na.approx}
#' @param maxgap When interpolate missing values with \code{na.approx}.
#'                 Maximum number of consecutive NAs to fill. Defaut to Inf.
#' @param type_aggr \code{character} Type of aggregation
#' \itemize{
#'  \item{"first"}{ : Date/Time result is equal to minimum of sequence, and this minimum is included in aggregation}
#'  \item{"last"}{ : Date/Time result is equal to maximum of sequence, and this maximum is included in aggregation}
#'}
#' 
#' @param width \code{character}, the width of the chart container. For \code{amChartsOutput}.
#' @param height \code{character}, the height of the chart container. For \code{amChartsOutput}.
#' 
#' @param main \code{character}, title.
#' @param ylab \code{character}, value axis label.
#' @param color \code{character}, color of series (in hexadecimal).
#' @param type \code{character}, Type of graph. Possible values are : "line" (default),
#'  "column", "step", "smoothedLine"
#' @param bullet \code{character}, point shape. Possible values are : "diamond", "square", 
#' "bubble",  "yError", "xError", "round", "triangleLeft", "triangleRight", "triangleUp"
#' @param bulletSize \code{numeric}, size of bullet.
#' @param linetype \code{numeric}, line type, 0 : solid, number : dashed length 
#' @param linewidth \code{numeric}, line width.
#' @param fillAlphas \code{numeric}, fill. Between 0 (no fill) to 1.
#' @param precision \code{numeric}, default set to  1.
#' @param connect \code{logical}, default set to  FALSE. Specifies whether to connect data points if data is missing.
#' @param export \code{logical}, default set to  FALSE. TRUE to display export feature.
#' @param legend \code{logical}, enabled or not legend ? Defaut to TRUE.
#' @param legendPosition \code{character}, legend position. Possible values are :
#' "left", "right", "bottom", "top"
#' @param legendHidden \code{logical} hide some series on rendering ? Defaut to FALSE
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
#' @param dataDateFormat \code{character} Data date format. Default to 'YYYY-MM-DD JJ:NN:ss'. See \code{\link{amTimeSeries}}.
#' @param categoryBalloonDateFormats \code{list} Date format objects for chart cursor. See \code{\link{amTimeSeries}}.
#' @param dateFormats \code{list} Date format objects for x-axis. See \code{\link{amTimeSeries}}.
#' 
#' @return a reactive expression with aggregate data and ts
#' 
#' @name rAmCharts-shinymodules-ts
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' library(shiny)
#' library(rAmCharts)
#' 
#' # number of points
#' n <- 1000000
#' data <- data.frame(date = seq(c(ISOdate(1999,12,31)), by = "5 min", length.out = n),
#'                           value = rnorm(n, 100, 50))
#' 
#' # maximun of points in javascript
#' max_points <- 1000
#' 
#' # Call module in UI
#' ui <- fluidPage(
#'   rAmChartsTimeSeriesUI("ts_1", height = "600px"),
#'   h4(textOutput("ts"))
#' )
#' 
#' # Define server
#' server <- function(input, output) {
#'   
#'   # Call module in server
#'   res <- callModule(rAmChartsTimeSeriesServer, "ts_1", reactive(data), reactive("date"), 
#'      reactive("value"), maxPoints = shiny::reactive(max_points),
#'      main = reactive("Example of rAmChartsTimeSeries module"),
#'      color = reactive("red"), periodFieldsSelection = reactive(TRUE)
#'    )
#'    
#'   # show module return and print ts
#'   output$ts <- renderText({
#'     print(res())
#'     paste0("Current ts : ", res()$ts)
#'   })
#'  
#'}
#'
#'# Run the application 
#'shinyApp(ui = ui, server = server)
#' 
#' }
#' 
#' @export
rAmChartsTimeSeriesUI <- function(id, width = "100%", height = "400px") {
  ns <- shiny::NS(id)
  amChartsOutput(ns("am_ts_module"), width = width, height = height)  
}

#' @rdname rAmCharts-shinymodules-ts
#' @export
rAmChartsTimeSeriesServer <- function(input, output, session, data, 
                                      col_date, col_series, maxPoints = shiny::reactive(600), tz  = shiny::reactive("UTC"),
                                      ts = shiny::reactive(c("5 min",  "10 min", "30 min", "hour", "3 hour", "12 hour", "day", "week", "month", "year")),
                                      fun_aggr = shiny::reactive("mean"), treat_missing = shiny::reactive(FALSE), maxgap = shiny::reactive(Inf), 
                                      type_aggr = shiny::reactive("first"), 
                                      main = shiny::reactive(""), 
                                      ylab = shiny::reactive(""),
                                      color = shiny::reactive(c("#2E2EFE", "#31B404", "#FF4000", "#AEB404")), 
                                      type = shiny::reactive(c("line")),
                                      bullet = shiny::reactive(NULL),
                                      bulletSize = shiny::reactive(2), 
                                      linetype = shiny::reactive(c(0, 5, 10, 15, 20)), 
                                      linewidth = shiny::reactive(c(1, 1, 1, 1, 1, 1)), 
                                      fillAlphas = shiny::reactive(0), 
                                      precision = shiny::reactive(1), 
                                      connect = shiny::reactive(FALSE),
                                      export = shiny::reactive(FALSE),
                                      legend = shiny::reactive(TRUE), 
                                      legendPosition = shiny::reactive("bottom"), 
                                      legendHidden = shiny::reactive(FALSE),
                                      ZoomButton = shiny::reactive(data.frame(Unit = "MAX", multiple = 1, label = "All")),
                                      ZoomButtonPosition = shiny::reactive("bottom"), 
                                      periodFieldsSelection = shiny::reactive(FALSE),
                                      scrollbar = shiny::reactive(TRUE), 
                                      scrollbarPosition = shiny::reactive("bottom"), 
                                      scrollbarHeight = shiny::reactive(40),
                                      scrollbarGraph = shiny::reactive(NULL), 
                                      cursor = shiny::reactive(TRUE), 
                                      cursorValueBalloonsEnabled = shiny::reactive(TRUE),
                                      creditsPosition = shiny::reactive("top-right"), 
                                      group = shiny::reactive(NULL),
                                      dataDateFormat = shiny::reactive('YYYY-MM-DD JJ:NN:ss'),
                                      categoryBalloonDateFormats = shiny::reactive(list(list(period = 'YYYY', format = 'YYYY'),
                                                                                        list(period='MM', format = 'YYYY-MM'), 
                                                                                        list(period = 'WW', format = 'YYYY-MM-DD'),
                                                                                        list(period='DD', format = 'YYYY-MM-DD'), 
                                                                                        list(period = 'hh', format = 'YYYY-MM-DD JJ:NN'),
                                                                                        list(period='mm', format = 'YYYY-MM-DD JJ:NN'), 
                                                                                        list(period = 'ss', format = 'YYYY-MM-DD JJ:NN:ss'),
                                                                                        list(period='fff', format = 'YYYY-MM-DD JJ:NN:ss'))),
                                      
                                      dateFormats = shiny::reactive(list(list(period = 'YYYY', format = 'YYYY'),
                                                                         list(period='MM', format = 'MMM'), 
                                                                         list(period = 'WW', format = 'MMM DD'),
                                                                         list(period='DD', format = 'MMM DD'), 
                                                                         list(period = 'hh', format = 'JJ:NN'),
                                                                         list(period='mm', format = 'JJ:NN'), 
                                                                         list(period = 'ss', format = 'JJ:NN:ss'),
                                                                         list(period='fff', format = 'JJ:NN:ss')))) {
  
  ns <- session$ns
  
  ctrl_data <- shiny::reactiveValues(data = NULL, ts = NULL, zoom = NULL, cpt = 0)
  
  output$am_ts_module <- renderAmCharts({
    data <- data()
    if(!is.null(data)){
      init_data <- getCurrentStockData(data, col_date = col_date(), col_series = unlist(col_series()), 
                                       maxPoints = maxPoints(), tz = tz(), ts = ts(), 
                                       fun_aggr = fun_aggr(), treat_missing = treat_missing(), 
                                       maxgap = maxgap(), type_aggr = type_aggr())
      
      ctrl_data$zoom <- NULL
      ctrl_data$data <- init_data$data
      ctrl_data$ts <- init_data$ts
      
      tmp_am <- amTimeSeries(data = init_data$data, maxSeries = maxPoints()+10, is_ts_module = TRUE,
                             col_date = col_date(), col_series = col_series(),
                             main = main(), ylab = ylab(), color = color(), 
                             type = type(), 
                             bullet = bullet(),
                             bulletSize = bulletSize(), 
                             linetype = linetype(), 
                             linewidth = linewidth(), 
                             fillAlphas = fillAlphas(), 
                             precision = precision(), 
                             connect = connect(),
                             export = export(),
                             legend = legend(), 
                             legendPosition = legendPosition(), 
                             legendHidden = legendHidden(),
                             ZoomButton = ZoomButton(),
                             ZoomButtonPosition = ZoomButtonPosition(), 
                             periodFieldsSelection = periodFieldsSelection(),
                             scrollbar = scrollbar(), 
                             scrollbarPosition = scrollbarPosition(), 
                             scrollbarHeight = scrollbarHeight(),
                             scrollbarGraph = scrollbarGraph(), 
                             cursor = cursor(), 
                             cursorValueBalloonsEnabled = cursorValueBalloonsEnabled(),
                             creditsPosition = creditsPosition(), 
                             group = group(), 
                             dataDateFormat = dataDateFormat(),
                             categoryBalloonDateFormats = categoryBalloonDateFormats(), 
                             dateFormats = dateFormats(),
                             groupToPeriods = init_data$ts)
      
      tmp_am <- addListener(tmp_am, "zoomed", paste0("function (event) {
                                                     var zoomed_event = event.chart.events.zoomed;
                                                     event.chart.events.zoomed = [];
                                                     event.chart.zoom(event.chart.previousStartDate, event.chart.previousEndDate);
                                                     event.chart.events.zoomed = zoomed_event;
                                                     Shiny.onInputChange('", ns("curve_zoom"), "', {start : event.startDate, end : event.endDate});
    }"))
      tmp_am
    }
    
  })
  
  shiny::observe({
    ctrl_data$zoom <- input$curve_zoom
  })
  
  shiny::observe({
    cur_zoom <- ctrl_data$zoom
    all_data <- shiny::isolate(data())
    if(!is.null(all_data) & !is.null(cur_zoom)){
      new_data <- getCurrentStockData(all_data, zoom = cur_zoom, col_date = col_date(), col_series = unlist(col_series()), 
                                      maxPoints = maxPoints(), tz = tz(), ts = ts(), fun_aggr = fun_aggr(), 
                                      treat_missing = treat_missing(), maxgap = maxgap(), type_aggr = type_aggr())
      
      if(!is.null(new_data)){
        ctrl_data$data <- new_data$data
        ctrl_data$ts <- new_data$ts
        
        session$sendCustomMessage("amChartStockModuleChangeData", 
                                  list(ns("am_ts_module"), jsonlite::toJSON(new_data$data), jsonlite::toJSON(new_data$ts)))
      } else {
        shiny::showModal(shiny::modalDialog(
          title = "Aggregation : no data available into subset",
          "Use directly mouse zoom rather than zoom button", 
          easyClose = TRUE, footer = NULL
        ))
      }
    }
  })
  
  res_data <- shiny::reactive({
    list(data = ctrl_data$data, ts = ctrl_data$ts, zoom = ctrl_data$zoom)
  })
  
  return(res_data)
}

#' Get data in shiny module
#' 
#' @param data : data.frame to transform
#' @param col_date : Date column name, default to "date".
#'                   Must be "POSIXct" or "CET24" colum
#' @param col_series : Column name of quantitative variable(s) to be
#'                     transformed. Default to setdiff(colnames(data), "date") 
#' @param zoom : List for init subset. NULL to keep all
#' @param maxPoints : Maximal number of rows in results
#' @param ts : Increment of the sequence. Default to "10 min".
#'             Can be a number, in seconds, or a character string
#'             containing one of "min", "hour", "day".
#'             This can optionally be preceded by a positive integer
#'             and a space
#' @param tz : Timezone of result. Defaut to "UTC".
#' @param fun_aggr : Aggregation function to use ("min", "max", "sum", "mean", "first", "last").
#'                   Default to "mean".
#' @param treat_missing : Boolean. Default to FALSE
#'                        Whether or not to interpolate missing values ?
#'                        see \code{na.approx}
#' @param maxgap : When interpolate missing values with \code{na.approx}.
#'                 Maximum number of consecutive NAs to fill. Defaut to Inf.
#' @param type_aggr : Character. Type of aggregation
#' \itemize{
#'  \item{"first"}{ : Date/Time result is equal to minimum of sequence, and this minimum is included in aggregation}
#'  \item{"last"}{ : Date/Time result is equal to maximum of sequence, and this maximum is included in aggregation}
#'}
#' 
#' @export
#' 
getCurrentStockData <- function(data, col_date, col_series, zoom = NULL, maxPoints = 1000, 
                                tz  = "UTC",
                                ts = c("5 min",  "10 min", "30 min", "hour", "3 hour", "12 hour", "day", "week", "month", "year"),
                                fun_aggr = "mean", treat_missing = FALSE, maxgap = Inf, type_aggr = "first"){
  
  if(!isTRUE(all.equal("data.frame", class(data)))){
    data <- data.frame(data)
  }
  
  if(!is.null(zoom)){
    start_time <- lubridate::floor_date(as.POSIXct(zoom$start, format = "%Y-%m-%dT%T", tz = "UTC"), "day")
    end_time <- lubridate::ceiling_date(as.POSIXct(zoom$end, format = "%Y-%m-%dT%T", tz = "UTC"), "day")
    tmp_data <- data[data[[col_date]] >= start_time & data[[col_date]] <= end_time, ]
  } else {
    start_time <- lubridate::floor_date(data[[col_date]][1], "day")
    end_time <- lubridate::ceiling_date(data[[col_date]][nrow(data)], "day")
    tmp_data <- data
  }
  
  if(nrow(tmp_data) > 0){
    # ts to second
    ts_seconds <- sapply(ts, function(x){
      tmp_range <- seq(c(ISOdate(2000,3,20)), by = x, length.out = 2)
      difftime(tmp_range[2], tmp_range[1], units = "secs")
    })
    
    difft <- difftime(end_time, start_time)
    
    # difference en secondes
    diff_secs <- as.numeric(difft, units = "secs")
    
    target_ts <- ts[which(ts_seconds >= diff_secs/maxPoints)[1]]
    
    # nouvelle data amCharts
    am_data <- getTransformTS(tmp_data, col_date = col_date, col_series = col_series, tz = tz, treat_missing = treat_missing, 
                              ts = target_ts, fun_aggr = fun_aggr, type_aggr = type_aggr, maxgap = maxgap)
    
    # first and last row in days to keep zoom possible
    first_row <- data[1, c(col_date, col_series)]
    
    lapply(col_series, function(x){
      first_row[[x]] <<- NA
    })
    first_row[[col_date]] <- lubridate::floor_date(data[[col_date]][1], "day")
    last_row <- first_row
    last_row[[col_date]] <- lubridate::ceiling_date(data[[col_date]][nrow(data)], "day")
    
    am_data <- rbind(first_row, am_data, last_row)
    if(am_data[[col_date]][1] == am_data[[col_date]][2]){
      am_data <- am_data[-1, ]
    }
    if(am_data[[col_date]][nrow(am_data)] == am_data[[col_date]][nrow(am_data) - 1]){
      am_data <- am_data[-nrow(am_data), ]
    }
    
    # ts amCharts
    am_ts <- gsub("sec$", "ss", target_ts)
    am_ts <- gsub("min$", "mm", am_ts)
    am_ts <- gsub("hour$", "hh", am_ts)
    am_ts <- gsub("day$", "DD", am_ts)
    am_ts <- gsub("week$", "WW", am_ts)
    am_ts <- gsub("month$", "MM", am_ts)
    am_ts <- gsub("year$", "YY", am_ts)
    am_ts <- gsub("[[:space:]]*", "", am_ts)
    
    return(list(data = am_data, ts = am_ts))
  } else {
    return(NULL)
  }
}
#' Transform quantitative variables.
#' 
#' Transform quantitative variables. Aggregate or interpolate time series data.
#' 
#' @param data : data.frame to transform
#' @param col_date : Date column name, default to "date".
#'                   Must be "POSIXct"
#' @param col_series : Column name of quantitative variable(s) to be
#'                     transformed. Default to setdiff(colnames(data), "date") 
#' @param col_by : Column name of a optionnal grouping variable. Default to NULL
#' @param ts : Increment of the sequence. Default to "10 min".
#'             Can be a number, in seconds, or a character string
#'             containing one of "min", "hour", "day".
#'             This can optionally be preceded by a positive integer
#'             and a space
#' @param tz : Timezone of result. Defaut to "UTC".
#' @param fun_aggr : Aggregation function to use ("min", "max", "sum", "mean", "first", "last").
#'                   Default to "mean".
#' @param treat_missing : Boolean. Default to FALSE
#'                        Whether or not to interpolate missing values ?
#'                        see \code{na.approx}
#' @param control_date : Boolean. Control full data sequence ? Defaut to TRUE and set to TRUE if treat_missing
#' @param maxgap : When interpolate missing values with \code{na.approx}.
#'                 Maximum number of consecutive NAs to fill. Defaut to Inf.
#' @param keep_last : Boolean. Keep last date/time value after interpolation ?
#' @param type_aggr : Character. Type of aggregation
#' \itemize{
#'  \item{"first"}{ : Date/Time result is equal to minimum of sequence, and this minimum is included in aggregation}
#'  \item{"last"}{ : Date/Time result is equal to maximum of sequence, and this maximum is included in aggregation}
#'}
#'
#' @param showwarn : Boolean. Show warnings ?
#' 
#' @return a data.frame
#' 
#' 
#' 
#' @export
#' @importFrom zoo na.approx
#' @import data.table
#'

# data
# col_date  = "date"
# col_series = setdiff(colnames(data), col_date)
# ts = "week"
# tz = "UTC"
# fun_aggr = "mean"
# treat_missing = TRUE
# maxgap = Inf
# keep_last = TRUE
# type_aggr = "first"
# 
# getTransformTS(data)
getTransformTS <- function(data, 
                           col_date  = "date",
                           col_series = setdiff(colnames(data), c(col_date, col_by)),
                           col_by = NULL, 
                           ts = "10 min", 
                           tz = "UTC",
                           fun_aggr = "mean", 
                           treat_missing = FALSE,
                           control_date = TRUE,
                           maxgap = Inf, 
                           keep_last = TRUE, 
                           type_aggr = "first", 
                           showwarn = FALSE){
  
  
  if(!showwarn){
    cur_warn <- options("warn")
    options(warn = -1)
  }
  
  if(treat_missing){
    control_date <- TRUE
  }
  
  ## data.table
  if("data.table" %in% class(data)){
    data <- data.frame(data)
  }
  
  ### Check function arguments ----------------------------
  if (!col_date %in% colnames(data)) {
    stop(paste0("Can't find ", col_date, " column in 'data'"))
  }
  
  if (any(!col_series %in% colnames(data))) {
    stop("Invalid 'col_series' argument")
  }
  
  if (!is.null(col_by) && (any(!col_by %in% colnames(data)) | length(col_by) > 1)) {
    stop("Invalid 'col_by' argument")
  }
  
  check_quanti <- sapply(data[, col_series, drop=FALSE], class)
  if (any(check_quanti %in% c("factor", "character", "logical"))) {
    stop("Some columns are not quantitative")
  }
  
  # add tmp_ before variables in col_series
  tmp_function <- function(x) {
    new_col_series <- paste0("tmp_", x)
    colnames(data) <<- gsub(paste0("^", x, "$"), new_col_series,
                            colnames(data))
    new_col_series
  }
  
  col_series <- sapply(col_series, tmp_function)
  
  # wanted time
  if ("character" %in% class(ts)) {
    wanted_time <- try(match.arg(gsub("^[[:digit:]]+[[:space:]]+","", ts), 
                                 choices = c("day", "hour", "min", "week", "month", "year")),
                       silent = TRUE)
    
    if ("try-error" %in% class(wanted_time)) {
      stop("Invalid 'ts' argument. Must use 'day', 'hour', 'min', 'week', 'month', 'year'")
    }
    
    # number ?
    check <- regexpr("[[:digit:]]+", ts)
    number_time       <- ifelse(check == -1, 1, as.numeric(regmatches(ts, check)))
    controle_time     <- c(day = 86400, hour = 3600, min = 60, week = 7*24*60*60, month = 30*24*60*60, year = 365*24*60*60) 
    wanted_time_level <- controle_time[wanted_time] * number_time
  } else {
    if (ts < 60 | ts > 24*60*60) {
      stop("Invalid 'ts' argument")
    }
    wanted_time_level <- ts
    wanted_time <- as.character(cut(ts, breaks = c(60, 3600, 86400,  Inf),
                                    labels = c("min", "hour", "day"),
                                    include.lowest = TRUE, right = FALSE))
    number_time <- switch(wanted_time,
                          min  = wanted_time_level / 60,
                          hour = wanted_time_level / 3600,
                          day  = wanted_time_level / 86400,
                          stop("Invalid ts argument"))
  }
  
  if(!isTRUE(all.equal(number_time, as.integer(number_time)))){
    stop("Must have integer time step per period")
  }
  
  # timezone consistency between in- and output
  
  if ("Date" %in% class(data[[col_date]])) {
    tzdate <- tz
    
    # donnees manquantes -> rajout de NA
    v_date <- seq(data[[col_date]][1],
                  data[[col_date]][nrow(data)], by = "day")
    
    if(is.null(col_by)){
      data_check <- data.table(v_date)
      colnames(data_check)[1] <- col_date
      data <- data.frame(merge(data.table(data), data_check, by = col_date, all = TRUE))
    } else {
      v_id <- unique(data[[col_by]])
      data_check <- data.table(rep(v_date, length(v_id)), rep(v_id, each = length(v_date)))
      colnames(data_check) <-  c(col_date, col_by)  
      data <- merge(data.table(data), data_check, by = c(col_date, col_by), all = TRUE)
      setorder(data, col_by, col_date)
      data <- data.frame(data)
    }
    control_date <- FALSE
    data[[col_date]] <- as.POSIXct(as.character(data[[col_date]]), tz = tzdate)
  }
  
  if (!"POSIXct" %in% class(data[[col_date]])) {
    stop("Date must be in POSIXct format, Date format")
  }
  
  # verification concordante et tz souhaite
  if (is.null(attr(data[[col_date]], "tzone"))) {
    attr(data[[col_date]], "tzone") <- tz
  } else if (attr(data[[col_date]], "tzone") != tz) {
    attr(data[[col_date]], "tzone") <- tz
  }
  
  treat_cet_as_utc <- FALSE
  if (wanted_time_level <= 3600 & attr(data[[col_date]], "tzone") == "CET") {
    attr(data[[col_date]], "tzone") <- "UTC"
    treat_cet_as_utc <- TRUE
  }
  
  initial_column_order <- colnames(data) 
  initial_column_order <- initial_column_order[initial_column_order
                                               %in% c(col_date, col_by, col_series)]
  current_tz <- attr(data[[col_date]], "tzone")
  
  # control time level, en faisant attention au chgmt d'heure
  tmp_function <- function(x) {
    as.double(difftime(data[[col_date]][x], data[[col_date]][x - 1],
                       tz = current_tz, units = "secs"))
  }
  current_time_level <- as.numeric(names(
    sort(table(sapply(2:min(10, nrow(data)), tmp_function)),
         decreasing = TRUE)[1]))
  
  
  if (control_date) {
    # donnees manquantes -> rajout de NA
    v_date <- seq(data[[col_date]][1],
                  data[[col_date]][nrow(data)],
                  by = current_time_level)
    
    if(is.null(col_by)){
      data_check <- data.table(v_date)
      colnames(data_check)[1] <- col_date
      data <- data.frame(merge(data.table(data), data_check, by = col_date, all = TRUE))
    } else {
      v_id <- unique(data[[col_by]])
      data_check <- data.table(rep(v_date, length(v_id)), rep(v_id, each = length(v_date)))
      colnames(data_check) <-  c(col_date, col_by)
      data <- merge(data.table(data), data_check, by = c(col_date, col_by), all = TRUE)
      setorderv(data, c(col_by, col_date), c(1, 1))
      data <- data.frame(data)
    }
  }
  
  if(type_aggr == "last"){
    data[[col_date]] <- data[[col_date]] - current_time_level
  }
  
  data <- data.table(data[[col_date]],
                     mtqdate = as.IDate(data[[col_date]], tz = current_tz),
                     mtqtime = as.ITime(data[[col_date]], tz = current_tz),
                     data[, c(col_by, col_series), drop = FALSE])
  
  setnames(data, "V1", col_date)
  
  # we compute only on columns with at least two non missing values
  tmp_function <- function(x) {
    data[!is.na(eval(parse(text = x))), .N]
  }
  col_series_na <- col_series[which(sapply(col_series, tmp_function) < 2)]
  
  # donnees manquantes -> extrapolation lineaire
  if (treat_missing) {
    treat_col_series <- setdiff(col_series, col_series_na)
    if (length(treat_col_series) > 0 ) {
      expr_extrapolation <- parse(
        text = paste0("c('", col_date, "','mtqdate', 'mtqtime', ",
                      paste(paste0("'", treat_col_series,"'"), collapse = ","),
                      ") := list(", col_date, ", mtqdate, mtqtime, ",
                      paste(paste0("zoo::na.approx(", treat_col_series,
                                   ", maxgap = ", maxgap, ", na.rm = FALSE)"),
                            collapse = ","), ")"))
      if(is.null(col_by)){
        data <- data[, eval(expr_extrapolation)]
      } else {
        data <- data[, eval(expr_extrapolation), by = col_by]
      }
    }
  } 
  
  ### Aggregation ----------------------------
  if (current_time_level < wanted_time_level) {
    if (!fun_aggr %in% c("mean", "max", "min", "sum", "first", "last")) {
      stop("Invalid 'fun_aggr', must be one of 'mean', 'max', 'min', 'sum', 'first' or 'last'")
    }
    
    if(wanted_time_level%%current_time_level != 0){
      stop("Aggregation only available in multiple time level looking initial data")
    }
    timefun <- ifelse(type_aggr == "first", "min", "max")
    
    if(fun_aggr %in% c("mean", "max", "min", "sum")){
      expr_transformation <- parse(text = paste0("list(", col_date, "=", timefun, "(", col_date, "),",
                                                 paste(paste0(col_series, "=", fun_aggr, "(", col_series,
                                                              ", na.rm = TRUE)"), collapse = ","), ",tmp_N = .N)"))
    } else if(fun_aggr == "first"){
      expr_transformation <- parse(text = paste0("list(", col_date, "=", timefun, "(", col_date, "),",
                                                 paste(paste0(col_series, "= head(", col_series,
                                                              "[!is.na(", col_series, ")], n = 1)"), collapse = ","), ",tmp_N = .N)"))
    } else if(fun_aggr == "last"){
      expr_transformation <- parse(text = paste0("list(", col_date, "=", timefun, "(", col_date, "),",
                                                 paste(paste0(col_series, "= tail(", col_series,
                                                              "[!is.na(", col_series, ")], n = 1)"), collapse = ","), ",tmp_N = .N)"))
    }
    
    
    if (wanted_time == "year") {
      if(number_time > 1){
        stop("Can't set 'year' superior to 1")
      }
      
      expr_group <- "list(year(mtqdate))"
    } else
      if (wanted_time == "month") {
        if(number_time > 1){
          stop("Can't set 'month' superior to 1")
        }
        
        expr_group <- "list(year(mtqdate), month(mtqdate))"
        
      } else if (wanted_time == "week") {
        if(number_time > 1){
          stop("Can't set 'week' superior to 1")
        }
        
        nearest_thursday2 <- dtthursday0(data$mtqdate)
        year2 <- year(nearest_thursday2)
        uni_january04 <- as.IDate(paste0(unique(year2), "-01-04"))
        uni_thursday2 <- dtthursday0(uni_january04)
        names(uni_thursday2) <- unique(year2)
        january042 <- uni_thursday2[as.character(year2)]
        first_thursday2 <- dtthursday0(january042)
        diffdays2 <- nearest_thursday2-first_thursday2
        week2 <- as.integer(diffdays2)%/%7 + 1
        
        data[, c('isoy', 'isow') := list(year2, week2)]
        expr_group <- "list(isoy, isow)"
        
        # expr_group <- parse(text = "list(year(mtqdate), isoweek(mtqdate))")
        # expr_group <- parse(text = "list(ISOweek(mtqdate))")
        # expr_group <- parse(text = "list(format(mtqdate, '%Y-W%V'))")
        
      } else if (wanted_time == "day") {
        if(number_time > 366){
          stop("Can't set 'day' superior to 366")
        }
        if (number_time == 1) {
          expr_group <- "list(mtqdate)"
        } else {    
          expr_group <- paste0("list(year(mtqdate), ",
                               "ctrl = mycut(as.data.table(yday(mtqdate)), ",
                               "breaks = c(",
                               paste(seq(1, 368, by = number_time), collapse = ","),
                               "))[, V1])")
        }
      } else if (wanted_time == "hour") {
        if(number_time > 24){
          stop("For more than 24 hours, please use 'day'")
        }
        if(24%%number_time != 0){
          stop("Only accept regular sequence")
        }
        if (number_time == 1) {
          expr_group <- "list(mtqdate, hour(mtqtime))"
        } else {    
          expr_group <- paste0("list(mtqdate, ",
                               "ctrl = mycut(as.data.table(hour(mtqtime)), ",
                               "breaks = c(",
                               paste(seq(0, 24, by = number_time), collapse = ","),
                               "))[, V1])")
        }
      } else if (wanted_time == "min") {
        if(number_time > 60){
          stop("For more than 60 min, please use 'hour'")
        }
        if(60%%number_time != 0){
          stop("Only accept regular sequence")
        }
        if (number_time == 1) {
          expr_group <- "list(mtqdate, hour(mtqtime), as.POSIXlt(mtqtime)$min)"
        } else {    
          expr_group <- paste0("list(mtqdate,hour(mtqtime), ",
                               "ctrl = mycut(as.data.table(as.POSIXlt(mtqtime)$min),",
                               "breaks = c(",
                               paste(seq(0, 60, by = number_time), collapse = ","),
                               "))[, V1])")
        }
      }
    
    if(is.null(col_by)){
      res <- data[, eval(expr_transformation), eval(parse(text = expr_group))]
    } else {
      expr_group <- gsub("list(", paste0("list(", col_by, ","), expr_group, fixed = TRUE)
      res <- data[, eval(expr_transformation), eval(parse(text = expr_group))]
    }
    
    
    if ("mtqdate" %in% colnames(res)) {
      res[, c("mtqdate") := NULL]
    }
    
    if ("mtqtime" %in% colnames(res)) {
      res[, c("mtqtime") := NULL]
    }
    
    # check first value
    if(res[2, get("tmp_N")] != res[1, get("tmp_N")] & showwarn){
      warning("Be carreful : first value seems to be compute on a incomplete sequence")
    }
    
    # check last value
    if(res[nrow(res), get("tmp_N")] != res[nrow(res)-1, get("tmp_N")] & showwarn){
      warning("Be carreful : last value seems to be compute on a incomplete sequence")
    }
    
    res[, c("tmp_N") := NULL]
    
    if(type_aggr == "last"){
      expr_transform <- paste0(col_date, ":=", col_date, "+", current_time_level)
      res[, eval(parse(text=expr_transform))] 
    }
    
    # tri
    if(is.null(col_by)){
      setorderv(res, col_date, 1)
    } else {
      setorderv(res, c(col_by, col_date), c(1, 1))
    }
    
    res <- as.data.frame(res)
    
  } else if (current_time_level > wanted_time_level) {
    if (wanted_time_level <= 60 * 60 | !current_tz %in% c("CET", "CET24")) {
      new_date_time <- seq.POSIXt(
        as.POSIXct(paste(data[1, get("mtqdate")], data[1, get("mtqtime")]), tz = current_tz),
        as.POSIXct(paste(data[nrow(data), get("mtqdate")], data[nrow(data), get("mtqtime")]),
                   tz = current_tz), by = wanted_time_level)
    } else {
      new_date_time <- (
        computeDateSequenceCET(
          date = c(as.POSIXct(paste(data[1, get("mtqdate")], data[1, get("mtqtime")]), tz = current_tz),
                   as.POSIXct(paste(data[nrow(data), get("mtqdate")],
                                    data[nrow(data), get("mtqtime")]), tz = current_tz)), 
          ts = wanted_time_level))
      attr(new_date_time, "tzone") <- current_tz
    }
    

    tmp_compute <- paste(col_series, " = ", ifelse(col_series %in% col_series_na, "NA", 
                                     paste0("stats::approx(x = ", col_date, ", y = ", col_series, ",xout = new_date_time)$y")))
    eval_transformation <- paste0("list(date = new_date_time, ", paste(tmp_compute, collapse = ", "), ")")

    if(is.null(col_by)){
      res <- data[, eval(parse(text = eval_transformation))]
    } else {
      res <- data[, eval(parse(text = eval_transformation)), by = col_by]
    }
    
    res <- as.data.frame(res)
    if (!keep_last) {
      res <- res[-nrow(res), ]
    }
    
  } else {
    res <- as.data.frame(data)
    if (!keep_last) {
      res <- res[-nrow(res), ]
    }
  }
  
  res <- res[, initial_column_order]
  
  # transform back UTC into CET
  if (treat_cet_as_utc) {
    attr(res[, col_date], "tzone") <- "CET"
  }
  
  # remove tmp_ before col_series
  tmp_function <- function(x) {
    new_col_series <- gsub("^tmp_", "", x)
    colnames(res) <<- gsub(paste0("^", x, "$"), new_col_series, colnames(res))
    invisible()
  }
  rename_quanti <- sapply(col_series, tmp_function)
  
  if(!showwarn){
    options(warn = cur_warn$warn)
  }
  
  res
}

mycut <- function(m, breaks) {
  tmp_function <- function(x) {
    if (x < (length(breaks) - 1)) {
      m[m >= breaks[x] & m < breaks[x + 1]] <<- x
    } else {
      m[m >= breaks[x] & m <= breaks[x + 1]] <<- x
    }
  }
  sapply(1:(length(breaks) - 1), tmp_function)
  m
}

# dtISOweekday <- function (date) 
# {
#   return(as.integer(wday(date) + 5)%%7 + 1)
# }

dtweekday0 <- function (date) {
  return(as.integer(data.table::wday(date) + 5)%%7)
}

dtthursday0 <- function (date) {
  return(date - dtweekday0(date) + 3)
}

computeDateSequenceCET <- function (date, ts){
  seq_date <- seq(date[1], date[length(date)], by = 60 * 60)
  regular_day_grid <- format(seq(as.POSIXct("1910-01-01 00:00:00", 
                                            tz = "UTC"), as.POSIXct("1910-01-01 23:50:00", tz = "UTC"), 
                                 by = ts), format = "%H:%M:%S")
  generateSummerTimeDayOfYear <- function(year) {
    hour_change <- hourChangeYear(year)
    gsub(" 02:", " 03:", paste(format(hour_change[[2]], "%Y-%m-%d"), 
                               regular_day_grid))
  }
  summer_time_day <- do.call("c", lapply(unique(format(date, 
                                                       "%Y")), generateSummerTimeDayOfYear))
  ind_keep <- unique(sort(c(which(format(seq_date, "%H:%M:%S") %in% 
                                    regular_day_grid), which(seq_date %in% as.POSIXct(summer_time_day, 
                                                                                      tz = "CET")))))
  seq_date <- seq_date[ind_keep]
  ind_dup <- which(duplicated(format(seq_date, "%Y%m%d %H:%M:%S"), 
                              fromLast = FALSE))
  if (length(ind_dup) > 0) {
    seq_date <- seq_date[-ind_dup]
  }
  attr(seq_date, "tzone") <- "CET"
  seq_date
}

hourChangeYear <- function (year) {
  start_date <- as.POSIXct(paste0(year, "-01-01"), tz = "CET")
  end_date <- as.POSIXct(paste0(year, "-12-31"), tz = "CET")
  dates <- as.POSIXlt(seq(from = start_date, to = end_date, 
                          by = 60 * 60L))
  hours <- dates$hour
  hours_diff <- hours[2:length(hours)] - hours[1:(length(hours) - 
                                                    1)]
  ind_winter_time <- which(hours_diff == 0)
  winter_time <- dates[c(ind_winter_time + 2)]
  ind_summer_time <- which(hours_diff == 2)
  summer_time <- dates[c(ind_summer_time + 1)]
  list(winter_time, summer_time)
}
