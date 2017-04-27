#' Shiny module to render large time-series data with live server-client aggregation
#'
#' 
#' @param id  character, used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#' @param input   standard, \code{shiny} input
#' @param output  standard, \code{shiny} output
#' @param session standard, \code{shiny} session
#' @param data : data.frame to transform
#' @param col_date : Date column name, default to "date".
#'                   Must be "POSIXct" or "CET24" colum
#' @param col_series : Column name of quantitative variable(s) to be
#'                     transformed. Default to setdiff(colnames(data), "date") 
#' @param zoom : List for init subset. NULL to keep all
#' @param maxPoints : Maximal number of rows in results
#' @param ts : All enabled aggregation. Default to c("5 min",  "10 min", "30 min", "hour", "3 hour", "12 hour", "day", "week", "month", "year").
#'             Can be a number, in seconds, or a character string
#'             containing one of "min", "hour", "day"....
#'             This can optionally be preceded by a positive integer
#'             and a space
#' @param tz : Timezone of result. Defaut to "UTC".
#' @param fun_aggr : Aggregation function to use ("min", "max", "sum", "mean").
#'                   Default to "mean".
#' @param treat_missing : Boolean. Default to FALSE
#'                        Whether or not to interpolate missing values ?
#'                        see \code{na.approx}
#' @param maxgap : When interpolate missing values with \code{na.approx}.
#'                 Maximum number of consecutive NAs to fill. Defaut to Inf.
#' @param type_aggr: Character. Type of aggregation
#' \itemize{
#'  \item{"first"}{ : Date/Time result is equal to minimum of sequence, and this minimum is included in aggregation}
#'  \item{"last"}{ : Date/Time result is equal to maximum of sequence, and this maximum is included in aggregation}
#'}
#' 
#' @return a reactive expression with aggregate data and ts
#' 
#' @name rAmCharts-shinymodules-ts
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # ui
#' rAmChartTimeSeriesUI("ts_1")
#' 
#' # server
#'callModule(rAmChartTimeSeriesServer, "ts_1", data_stock_2, "date", "value", 
#'  periodFieldsSelection = TRUE)
#' 
#' }
#' 
#' @export
rAmChartTimeSeriesUI <- function(id) {
  ns <- NS(id)
  amChartsOutput(ns("am_ts_module"))  
}

#' @rdname rAmCharts-shinymodules-ts
#' @export
rAmChartTimeSeriesServer <- function(input, output, session, data, 
                                     col_date, col_series, maxPoints = 600, tz  = "UTC",
                                     ts = c("5 min",  "10 min", "30 min", "hour", "3 hour", "12 hour", "day", "week", "month", "year"),
                                     fun_aggr = "mean", treat_missing = FALSE, maxgap = Inf, type_aggr = "first", ...) {
  
  ns <- session$ns
  
  ctp <- reactiveVal(0)
  
  output$am_ts_module <- renderAmCharts({
    init_data <- getCurrentStockData(data, col_date = "date", col_series = "value", maxPoints = maxPoints, tz = tz, ts = ts, 
                                      fun_aggr = fun_aggr, treat_missing = treat_missing, maxgap = maxgap, type_aggr = type_aggr)
    
    tmp_am <- amTimeSeries(init_data$data, "date", "value", maxSeries = maxPoints+10, groupToPeriods = NULL, is_ts_module = TRUE, ...)
    tmp_am <- addListener(tmp_am, "zoomed", paste0("function (event) {
                          var zoomed_event = event.chart.events.zoomed;
                          event.chart.events.zoomed = [];
                          event.chart.zoom(event.chart.previousStartDate, event.chart.previousEndDate);
                          event.chart.events.zoomed = zoomed_event;
                          Shiny.onInputChange('", ns("curve_zoom"), "', {start : event.startDate, end : event.endDate});
    }"))
    tmp_am
  })
  
  new_data <- reactive({
    input$curve_zoom
    cur_cpt <- isolate(ctp())
    if(cur_cpt > 0){
      new_data <- getCurrentStockData(data, zoom = input$curve_zoom, col_date = "date", col_series = "value", 
                                      maxPoints = maxPoints, tz = tz, ts = ts, fun_aggr = fun_aggr, 
                                      treat_missing = treat_missing, maxgap = maxgap, type_aggr = type_aggr)
      session$sendCustomMessage("amChartStockModuleChangeData", 
                                list(ns("am_ts_module"), jsonlite::toJSON(new_data$data), jsonlite::toJSON(new_data$ts)))
      new_data$zoom <- input$curve_zoom
    } else {
      new_data <- NULL
    }
    ctp(cur_cpt+1)
    new_data
    
  })

  return(new_data)
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
#' @param fun_aggr : Aggregation function to use ("min", "max", "sum", "mean").
#'                   Default to "mean".
#' @param treat_missing : Boolean. Default to FALSE
#'                        Whether or not to interpolate missing values ?
#'                        see \code{na.approx}
#' @param maxgap : When interpolate missing values with \code{na.approx}.
#'                 Maximum number of consecutive NAs to fill. Defaut to Inf.
#' @param type_aggr: Character. Type of aggregation
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
  
  if(!is.null(zoom)){
    start_time <- lubridate::floor_date(as.POSIXct(zoom$start, format = "%Y-%m-%dT%T", tz = "UTC"), "day")
    end_time <- lubridate::ceiling_date(as.POSIXct(zoom$end, format = "%Y-%m-%dT%T", tz = "UTC"), "day")
    tmp_data <- data[data$date >= start_time & data$date <= end_time, ]
  } else {
    start_time <- lubridate::floor_date(data$date[1], "day")
    end_time <- lubridate::ceiling_date(data$date[nrow(data)], "day")
    tmp_data <- data
  }
  
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
  am_data <- getAggregateTS(tmp_data, col_date = col_date, col_series = col_series, tz = tz, treat_missing = treat_missing, 
                                   ts = target_ts, fun_aggr = fun_aggr, type_aggr = type_aggr, maxgap = maxgap)
  
  am_data <- rbind(data.frame(date = lubridate::floor_date(data$date[1], "day"), value = NA), 
                   am_data[-c(1, nrow(am_data)), ], data.frame(date = lubridate::ceiling_date(data$date[nrow(data)], "day"), value = NA))
  
  # ts amCharts
  am_ts <- gsub("sec$", "ss", target_ts)
  am_ts <- gsub("min$", "mm", am_ts)
  am_ts <- gsub("hour$", "hh", am_ts)
  am_ts <- gsub("day$", "DD", am_ts)
  am_ts <- gsub("week$", "WW", am_ts)
  am_ts <- gsub("month$", "MM", am_ts)
  am_ts <- gsub("year$", "YY", am_ts)
  am_ts <- gsub("[[:space:]]*", "", am_ts)
  
  list(data = am_data, ts = am_ts)
}
#' Transform quantitative variables.
#' 
#' Transform quantitative variables. Aggregate or interpolate time series data.
#' 
#' @param data : data.frame to transform
#' @param col_date : Date column name, default to "date".
#'                   Must be "POSIXct" or "CET24" colum
#' @param col_series : Column name of quantitative variable(s) to be
#'                     transformed. Default to setdiff(colnames(data), "date") 
#' @param ts : Increment of the sequence. Default to "10 min".
#'             Can be a number, in seconds, or a character string
#'             containing one of "min", "hour", "day".
#'             This can optionally be preceded by a positive integer
#'             and a space
#' @param tz : Timezone of result. Defaut to "UTC".
#' @param fun_aggr : Aggregation function to use ("min", "max", "sum", "mean").
#'                   Default to "mean".
#' @param treat_missing : Boolean. Default to FALSE
#'                        Whether or not to interpolate missing values ?
#'                        see \code{na.approx}
#' @param maxgap : When interpolate missing values with \code{na.approx}.
#'                 Maximum number of consecutive NAs to fill. Defaut to Inf.
#' @param keep_last: Boolean. Keep last date/time value after interpolation ?
#' @param type_aggr: Character. Type of aggregation
#' \itemize{
#'  \item{"first"}{ : Date/Time result is equal to minimum of sequence, and this minimum is included in aggregation}
#'  \item{"last"}{ : Date/Time result is equal to maximum of sequence, and this maximum is included in aggregation}
#'}
#'
#' @param showwarn : Boolean. Show warnings ?
#' 
#' @return a data.frame
#' 
#' @examples 
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

getAggregateTS <- function(data, col_date  = "date",
                                  col_series = setdiff(colnames(data), col_date),
                                  ts = "10 min", tz = "UTC",
                                  fun_aggr = "mean", treat_missing = FALSE,
                                  maxgap = Inf, keep_last = TRUE, type_aggr = "first", 
                                  showwarn = FALSE){
  
  if(treat_missing){
    merge.date <- TRUE
  } else {
    merge.date <- FALSE
  }
  
  
  ### Check function arguments ----------------------------
  if (!col_date %in% colnames(data)) {
    stop(paste0("Can't find ", col_date, " column in 'data'"))
  }
  
  if (any(!col_series %in% colnames(data))) {
    stop("Invalid 'col_series' argument")
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
    data_check <- data.table(seq(data[[col_date]][1],
                                 data[[col_date]][nrow(data)], by = "day"))
    setnames(data_check, "V1", col_date)
    data <- base::merge(data, data_check, by = col_date, all = TRUE)
    merge.date <- FALSE
    data[[col_date]] <- as.POSIXct(as.character(data[[col_date]]), tz = tzdate)
  }
  
  if (!"POSIXct" %in% class(data[[col_date]])) {
    stop("Date must be in POSIXct format, Date format")
  }
  
  # verification concordante et tz souhaite
  if (attr(data[[col_date]], "tzone") != tz) {
    attr(data[[col_date]], "tzone") <- tz
  }
  
  treat_cet_as_utc <- FALSE
  if (wanted_time_level <= 3600 & attr(data[[col_date]], "tzone") == "CET") {
    attr(data[[col_date]], "tzone") <- "UTC"
    treat_cet_as_utc <- TRUE
  }
  
  initial_column_order <- colnames(data) 
  initial_column_order <- initial_column_order[initial_column_order
                                               %in% c(col_date, col_series)]
  current_tz <- attr(data[[col_date]], "tzone")
  
  # control time level, en faisant attention au chgmt d'heure
  tmp_function <- function(x) {
    as.double(difftime(data[[col_date]][x], data[[col_date]][x - 1],
                       tz = current_tz, units = "secs"))
  }
  current_time_level <- as.numeric(names(
    sort(table(sapply(2:min(10, nrow(data)), tmp_function)),
         decreasing = TRUE)[1]))
  
  
  if (merge.date) {
    # donnees manquantes -> rajout de NA
    data_check <- data.table(seq(data[[col_date]][1],
                                 data[[col_date]][nrow(data)],
                                 by = current_time_level))
    setnames(data_check, "V1", col_date)  
    data <- base::merge(data, data_check, by = col_date, all = TRUE)
  }
  
  if(type_aggr == "last"){
    data[[col_date]] <- data[[col_date]] - current_time_level
  }
  
  data <- data.table(data[[col_date]],
                     mtqdate = as.IDate(data[[col_date]], tz = current_tz),
                     mtqtime = as.ITime(data[[col_date]], tz = current_tz),
                     data[, col_series, drop = FALSE])
  
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
      data <- data[, eval(expr_extrapolation)]
    }
  } 
  
  ### Aggregation ----------------------------
  if (current_time_level < wanted_time_level) {
    if (!fun_aggr %in% c("mean", "max", "min", "sum")) {
      stop("Invalid 'fun_aggr', must be one of 'mean', 'max', 'min' or 'sum'")
    }
    
    if(wanted_time_level%%current_time_level != 0){
      stop("Aggregation only available in multiple time level looking initial data")
    }
    timefun <- ifelse(type_aggr == "first", "min", "max")
    expr_transformation <- parse(
      text = paste0("list(", col_date, "=", timefun, "(", col_date, "),",
                    paste(paste0(col_series, "=", fun_aggr, "(", col_series,
                                 ", na.rm = TRUE)"), collapse = ","), ",tmp_N = .N)"))
    
    
    if (wanted_time == "year") {
      if(number_time > 1){
        stop("Can't set 'year' superior to 1")
      }
      
      expr_group <- parse(text = "list(year(mtqdate))")
    } else
      if (wanted_time == "month") {
        if(number_time > 1){
          stop("Can't set 'month' superior to 1")
        }
        
        expr_group <- parse(text = "list(year(mtqdate), month(mtqdate))")
        
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
        expr_group <- parse(text = "list(isoy, isow)")
        
        # expr_group <- parse(text = "list(year(mtqdate), isoweek(mtqdate))")
        # expr_group <- parse(text = "list(ISOweek(mtqdate))")
        # expr_group <- parse(text = "list(format(mtqdate, '%Y-W%V'))")
        
      } else if (wanted_time == "day") {
        if(number_time > 366){
          stop("Can't set 'day' superior to 366")
        }
        if (number_time == 1) {
          expr_group <- parse(text = "list(mtqdate)")
        } else {    
          expr_group <- parse(
            text = paste0("list(year(mtqdate), ",
                          "ctrl = mycut(as.data.table(yday(mtqdate)), ",
                          "breaks = c(",
                          paste(seq(1, 368, by = number_time), collapse = ","),
                          "))[, V1])"))
        }
      } else if (wanted_time == "hour") {
        if(number_time > 24){
          stop("For more than 24 hours, please use 'day'")
        }
        if(24%%number_time != 0){
          stop("Only accept regular sequence")
        }
        if (number_time == 1) {
          expr_group <- parse(text = "list(mtqdate, hour(mtqtime))")
        } else {    
          expr_group <- parse(
            text = paste0("list(mtqdate, ",
                          "ctrl = mycut(as.data.table(hour(mtqtime)), ",
                          "breaks = c(",
                          paste(seq(0, 24, by = number_time), collapse = ","),
                          "))[, V1])"))
        }
      } else if (wanted_time == "min") {
        if(number_time > 60){
          stop("For more than 60 min, please use 'hour'")
        }
        if(60%%number_time != 0){
          stop("Only accept regular sequence")
        }
        if (number_time == 1) {
          expr_group <- parse(
            text = "list(mtqdate, hour(mtqtime), as.POSIXlt(mtqtime)$min)")
        } else {    
          expr_group <- parse(
            text = paste0("list(mtqdate,hour(mtqtime), ",
                          "ctrl = mycut(as.data.table(as.POSIXlt(mtqtime)$min),",
                          "breaks = c(",
                          paste(seq(0, 60, by = number_time), collapse = ","),
                          "))[, V1])"))
        }
      }
    res <- data[, eval(expr_transformation), eval(expr_group)] ## ???
    
    if ("mtqdate" %in% colnames(res)) {
      res[, mtqdate := NULL]
    }
    
    if ("mtqtime" %in% colnames(res)) {
      res[, mtqtime := NULL]
    }
    
    if(showwarn){
      # check first value
      if(res[2, tmp_N] != res[1, tmp_N]){
        warning("Be carreful : first value seems to be compute on a incomplete sequence")
      }
      
      # check last value
      if(res[nrow(res), tmp_N] != res[nrow(res)-1, tmp_N]){
        warning("Be carreful : last value seems to be compute on a incomplete sequence")
      }
    }
    res[, tmp_N := NULL]
    
    if(type_aggr == "last"){
      expr_transform <- paste0(col_date, ":=", col_date, "+", current_time_level)
      res[, eval(parse(text=expr_transform))] 
    }
    
    # tri
    setorderv(res, col_date, 1)
    res <- as.data.frame(res)
    
  } else if (current_time_level > wanted_time_level) {
    if (wanted_time_level <= 60 * 60 |
        !current_tz %in% c("CET", "CET24")) {
      new_date_time <- seq.POSIXt(
        as.POSIXct(paste(data[1, mtqdate], data[1, mtqtime]), tz = current_tz),
        as.POSIXct(paste(data[nrow(data), mtqdate], data[nrow(data), mtqtime]),
                   tz = current_tz),
        by = wanted_time_level)
    } else {
      new_date_time <- (
        computeDateSequenceCET(
          date = c(as.POSIXct(paste(data[1, mtqdate], data[1, mtqtime]),
                              tz = current_tz),
                   as.POSIXct(paste(data[nrow(data), mtqdate],
                                    data[nrow(data), mtqtime]),
                              tz = current_tz)), 
          ts = wanted_time_level))
      attr(new_date_time, "tzone") <- current_tz
    }
    
    res <- data.frame(tmpdate = new_date_time)
    tmp_function <- function(x) {
      if (!x%in%col_series_na) {
        res$tmp <<- approx(x    = data[, eval(parse(text = col_date))],
                           y    = data[, eval(parse(text = x))],
                           xout = new_date_time)$y
      } else {
        res$tmp <<- NA
      }
      colnames(res)[ncol(res)] <<- x
    }
    sapply(col_series, tmp_function)
    colnames(res)[1] <- col_date
    
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

dtISOweekday <- function (date) 
{
  return(as.integer(wday(dt_date) + 5)%%7 + 1)
}

dtweekday0 <- function (date) 
{
  return(as.integer(wday(date) + 5)%%7)
}

dtthursday0 <- function (date) 
{
  return(date - dtweekday0(date) + 3)
}
