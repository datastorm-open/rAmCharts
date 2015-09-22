#' @include classUnion.R
NULL

#' @param amBalloon \linkS4class{AmBalloon}.
#' Argument for method \code{setBalloon}.
#' @examples
#' setBalloon(.Object = amStockChart(), gridPosition = "start")
#' # ---
#' @rdname initialize-AmStockChart
setMethod(f = "setBalloon", signature = c("AmStockChart", "AmBalloonOrMissing"),
          definition = function(.Object, amBalloon = NULL, ...)
          {
            if (is.null(amBalloon) && !missing(...)) {
              amBalloon <- amBalloon(...)
            } else if (is.null(amBalloon) && missing(...)) {
              stop("You must either give argument 'amBalloon' or its properties")
            } else {}
            
            .Object@balloon <- listProperties(amBalloon)
            validObject(.Object)
            return(.Object)
          })

#' @details CategoryAxesSettings set's settings common for all CategoryAxes of StockPanels.
#' If you change a property after the chart is initialized,
#' you should call stockChart.validateNow() method in order for it to work.
#' If there is no default value specified, default value of CategoryAxis class will be used.
#' you should get this axis from the chart and set properties to this object.
#' @examples
#' setCategoryAxesSettings(.Object = amStockChart(), gridPosition = "start")
#' # ---
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setCategoryAxesSettings", def = function(.Object, ...) {standardGeneric("setCategoryAxesSettings")})
#' @rdname initialize-AmStockChart
setMethod(f = "setCategoryAxesSettings", signature = c("AmStockChart"),
          definition = function(.Object, ...)
          {
            .Object <- setProperties(.Object = .Object, categoryAxesSettings = list(...))
            validObject(.Object)
            return(.Object)
          })

#' @details ChartCursorSettings settings set's settings for chart cursor.
#' If you change a property after the chart is initialized,
#' you should call stockChart.validateNow() method in order for it to work.
#' If there is no default value specified, default value of ChartCursor class will be used.
#' @examples
#' setChartCursorSettings(.Object = amStockChart(), oneBallOnly = TRUE)
#' # ---
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setChartCursorSettings", def = function(.Object, ...) {standardGeneric("setChartCursorSettings")})
#' @rdname initialize-AmStockChart
setMethod(f = "setChartCursorSettings", signature = c("AmStockChart"),
          definition = function(.Object, ...)
          {
            .Object <- setProperties(.Object = .Object, chartCursorSettings = list(...))
            validObject(.Object)
            return(.Object)
          })

#' @param chartScrollbarSettings \linkS4class{ChartScrollbar}.
#' If you change a property after the chart is initialized,
#' you should call stockChart.validateNow() method in order for it to work.
#' If there is no default value specified, default value of ChartScrollbar class will be used.
#' @examples
#' setChartScrollbarSettings(.Object = amStockChart(), enabled = TRUE)
#' # equivalent to:
#' chartScrollbarSettings_obj <- chartScrollbarSettings()
#' setChartScrollbarSettings(.Object = amStockChart(),
#'                           chartScrollbarSettings = chartScrollbarSettings_obj)
#' # ---
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setChartScrollbarSettings", def = function(.Object, chartScrollbarSettings = NULL, ...) {standardGeneric("setChartScrollbarSettings")})
#' @rdname initialize-AmStockChart
setMethod(f = "setChartScrollbarSettings", signature = c("AmStockChart", "ChartScrollbarOrMissing"),
          definition = function(.Object, chartScrollbarSettings = NULL, ...)
          {
            if (is.null(chartScrollbarSettings) && !missing(...)) {
              chartScrollbarSettings <- chartScrollbar(...)
            } else if (is.null(chartScrollbarSettings) && missing(...)) {
              stop("You must provide either argument 'chartScrollbarSettings' or its properties")
            } else {}
            
            .Object <- setProperties(.Object = .Object,
                                     chartScrollbarSettings = listProperties(chartScrollbarSettings))
            validObject(.Object)
            return(.Object)
          })

# > @comparedDataSets : setters ####

#' @examples
#' comparedDataSets_ls <- list(dataSet(compared = TRUE), dataSet(compared = TRUE))
#' setComparedDataSets(.Object = amStockChart(), comparedDataSets = comparedDataSets_ls)
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setComparedDataSets", def = function(.Object, comparedDataSets) {standardGeneric("setComparedDataSets")})
#' @rdname initialize-AmStockChart
setMethod(f = "setComparedDataSets", signature = c("AmStockChart"),
          definition = function(.Object, comparedDataSets)
          {
            rightClassElements <- prod(sapply(comparedDataSets, function(element) {is(element, "DataSet")}))
            if (!rightClassElements) {
              stop("[setComparedDataSets]: each element of comparedDataSets must be of class DataSet")
            } else {}
            .Object@comparedDataSets <- lapply(comparedDataSets, listProperties)
            validObject(.Object)
            return(.Object)
          })


#' @param dataSet \linkS4class{DataSet}.
#' @examples
#' addComparedDataSet(.Object = amStockChart(), compared = TRUE)
#' # ---
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "addComparedDataSet", def = function(.Object, dataSet = NULL, ...) {standardGeneric("addComparedDataSet")})
#' @rdname initialize-AmStockChart
setMethod(f = "addComparedDataSet", signature = c("AmStockChart", "DataSetOrMissing"),
          definition = function(.Object, dataSet = NULL, ...)
          {
            if (is.null(dataSet) && !missing(...)) {
              dataSet <- dataSet(...)
            } else if (is.null(dataSet) && missing(...)) {
              stop("You must either give argument 'dataSet' or its properties")
            } else {}
            
            .Object@comparedDataSets <- rlist::list.append(.Object@comparedDataSets,
                                                           listProperties(dataSet))
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' dataSets_ls <- list(dataSet(compared = FALSE), dataSet(compared = FALSE))
#' setDataSets(.Object = amStockChart(), dataSets = dataSets_ls)
#' # ---
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setDataSets", def = function(.Object, dataSets) {standardGeneric("setDataSets")})
#' @rdname initialize-AmStockChart
setMethod(f = "setDataSets", signature = c("AmStockChart"),
          definition = function(.Object, dataSets)
          {
            rightClassElements <- prod(sapply(dataSets, function(element) {is(element, "DataSet")}))
            if (!rightClassElements) {
              stop("[setDataSets]: each element of dataSets must be of class DataSet")
            } else {}
            .Object@dataSets <- lapply(dataSets, listProperties)
            validObject(.Object)
            return(.Object)
          })


#' @examples
#' addDataSet(.Object = amStockChart(), compared = FALSE)
#' # equivalent to:
#' dataSet_obj <- dataSet(compared = FALSE)
#' addDataSet(.Object = amStockChart(), dataSet = dataSet_obj)
#' # ---
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "addDataSet", def = function(.Object, dataSet = NULL, ...) {standardGeneric("addDataSet")})
#' @rdname initialize-AmStockChart
setMethod(f = "addDataSet", signature = c("AmStockChart", "DataSetOrMissing"),
          definition = function(.Object, dataSet = NULL, ...)
          {
            if (is.null(dataSet) && !missing(...)) {
              dataSet <- dataSet(...)
            } else if (is.null(dataSet) && !missing(...)) {
              stop("You must either give argument 'dataSet' or its properties")
            } else {}
            
            .Object@dataSets <- rlist::list.append(.Object@dataSets,
                                                   listProperties(dataSet))
            validObject(.Object)
            return(.Object)
          })

#' @details You can add it if you have more than one data set and want users
#' to be able to select/compare them.
#' @examples
#' setDataSetSelector(.Object = amStockChart(), width = 180)
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setDataSetSelector", def = function(.Object, ...) {standardGeneric("setDataSetSelector")})
#' @rdname initialize-AmStockChart
setMethod(f = "setDataSetSelector", signature = c("AmStockChart"),
          definition = function(.Object, ...)
          {
            .Object <- setProperties(.Object = .Object, dataSetSelector = list(...))
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' setLegendSettings(.Object = amStockChart(), equalWidths = TRUE)
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setLegendSettings", def = function(.Object, ...) {standardGeneric("setLegendSettings")})
#' @rdname initialize-AmStockChart
setMethod(f = "setLegendSettings", signature = c("AmStockChart"),
          definition = function(.Object, ...)
          {
            .Object <- setProperties(.Object = .Object, legendSettings = list(...))
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' setMainDataSet(.Object = amStockChart(), showInCompare = TRUE)
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setMainDataSet", def = function(.Object, dataSet = NULL, ...) {standardGeneric("setMainDataSet")})
#' @rdname initialize-AmStockChart
setMethod(f = "setMainDataSet", signature = c("AmStockChart", "DataSetOrMissing"),
          definition = function(.Object, dataSet = NULL, ...)
          {
            if (is.null(dataSet) && !missing(...)) {
              dataSet <- dataSet(...)
            } else if (is.null(dataSet) && !missing(...)) {
              stop("You must either give argument 'dataSet' or its properties")
            } else {}
            
            .Object@mainDataSet <- listProperties(dataSet)
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' panels_ls <- list(stockPanel(compared = TRUE), stockPanel(compared = TRUE))
#' setPanels(.Object = amStockChart(), panels = panels_ls)
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setPanels", def = function(.Object, panels) {standardGeneric("setPanels")})
#' @rdname initialize-AmStockChart
setMethod(f = "setPanels", signature = c("AmStockChart", "list"),
          definition = function(.Object, panels)
          {
            rightClassElements <- prod(sapply(panels, function(element) {is(element, "StockPanel")}))
            if (!rightClassElements) {
              stop("[setPanels]: each element of panels must be of class Panel")
            } else {}
            .Object@panels <- lapply(panels, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @param panel \linkS4class{StockPanel}.
#' @examples
#' addPanel(.Object = amStockChart(), allowTurningOff = TRUE)
#' # equivalent to:
#' panel_obj <- panel(allowTurningOff = TRUE)
#' addPanel(.Object = amStockChart(), panel = panel_obj)
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "addPanel", def = function(.Object, panel = NULL, ...) {standardGeneric("addPanel")})
#' @rdname initialize-AmStockChart
setMethod(f = "addPanel", signature = c("AmStockChart", "StockPanelOrMissing"),
          definition = function(.Object, panel = NULL, ...)
          {
            if (is.null(panel) && !missing(...)) {
              panel <- panel(...)
            } else if (is.null(panel) && !missing(...)) {
              stop("You must either give panel argument or its properties")
            } else {}
            .Object@panels <- rlist::list.append(.Object@panels, listProperties(panel))
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' setPanelsSettings(.Object = amStockChart(), backgroundAlpha = 0)
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setPanelsSettings", def = function(.Object, ...) {standardGeneric("setPanelsSettings")})
#' @rdname initialize-AmStockChart
setMethod(f = "setPanelsSettings", signature = c("AmStockChart"),
          definition = function(.Object, ...)
          {
            .Object <- setProperties(.Object = .Object, panelsSettings = list(...))
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' setPeriodSelector(.Object = amStockChart(), dateFormat = "DD-MM-YYYY")
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setPeriodSelector", def = function(.Object, periodSelector = NULL, ...) {standardGeneric("setPeriodSelector")})
#' @rdname initialize-AmStockChart
setMethod(f = "setPeriodSelector", signature = c("AmStockChart", "PeriodSelectorOrMissing"),
          definition = function(.Object, periodSelector = NULL, ...)
          {
            if (is.null(periodSelector) && !missing(...)) {
              periodSelector <- periodSelector(...)
            } else if (is.null(periodSelector) && missing(...)) {
              stop("You must either give argument 'periodSelector' or its properties.")
            } else {}
            .Object@periodSelector <- listProperties(periodSelector)
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' setStockEventsSettings(.Object = amStockChart(), backgroundAlpha = 1)
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setStockEventsSettings", def = function(.Object, ...) {standardGeneric("setStockEventsSettings")})
#' @rdname initialize-AmStockChart
setMethod(f = "setStockEventsSettings", signature = c("AmStockChart"),
          definition = function(.Object, ...)
          {
            .Object <- setProperties(.Object = .Object, stockEventsSettings = list(...))
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' setValueAxesSettings(.Object = amStockChart(), autoGridCount = "TRUE")
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setValueAxesSettings", def = function(.Object, ...) {standardGeneric("setValueAxesSettings")})
#' @rdname initialize-AmStockChart
setMethod(f = "setValueAxesSettings", signature = c("AmStockChart"),
          definition = function(.Object, ...)
          {
            .Object <- setProperties(.Object = .Object, valueAxesSettings = list(...))
            validObject(.Object)
            return(.Object)
          })