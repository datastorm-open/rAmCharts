#' @include AmChart.R StockPanel.R
NULL

#' @title AmStockChart
#' @author Dataknowledge
#' @field allLabels \code{list} of \linkS4class{Label}.
#' 
#' @field arrows \code{list} of class \linkS4class{GaugeArrow}.
#' 
#' @field axes
#' Object of class \code{"list"}
#' containing object of class \code{\linkS4class{GaugeAxis}}.
#' 
#' @field balloon
#' Object of class \code{"list"}.
#' List of an \code{\linkS4class{AmBalloon}} class properties.
#' Creates the balloons (tooltips) of the chart,
#' It follows the mouse cursor when you roll-over the data items.
#' The framework generates the instances automatically you only need to adjust
#' the appearance to your needs.
#' 
#' @field categoryAxis
#' Object of class \code{"list"}.
#' List of a \code{\linkS4class{CategoryAxis}} properties.
#' Read-only. Chart creates category axis itself.
#' If you want to change some properties,
#' you should get this axis from the chart and set properties to this object.
#' 
#' @field categoryField
#' Object of class \code{"character"}.
#' Category field name tells the chart the name of the field in your dataProvider object
#' which will be used for category axis values.
#' 
#' @field ChartCursor
#' Object of class \code{"list"}.
#' List of a \code{\linkS4class{ChartCursor}} class properties.
#' Properties of the chart's cursor.
#' 
#' @field ChartScrollbar
#' Object of class \code{"list"}.
#' List of a \code{\linkS4class{ChartScrollbar}} class properties.
#' Properties of chart's scrollbar.
#' 
#' @field creditsPosition
#' Object of class \code{"character"},
#' specifying position of link to amCharts site.
#' Allowed values are: top-left, top-right, bottom-left and bottom-right.
#' 
#' @field dataProvider
#' Object of class \code{"list"}, containing the data.
#' Use providing method toList* to convert a \code{data.frame}.
#' 
#' @field graphs
#' Object of class \code{list}.  List of AmGraphs properties
#' See \code{\linkS4class{AmGraph}} class.
#' Creates the visualization of the data in following types: line, column, step line,
#' smoothed line, olhc and candlestick.
#' 
#' @field graph
#' Object of class \code{\linkS4class{AmGraph}}.
#' Graph of a Gantt chart. Gant chart actually creates multiple graphs (separate for each segment).
#' Properties of this graph are passed to each of the created graphs
#' - this allows you to control the look of segments.
#' 
#' @field guides
#' Object of class \code{list}.
#' Each elemnt must be of class \code{\linkS4class{Guide}}.
#' Instead of adding guides to the axes, you can push all of them to this array.
#' In case guide has category or date defined, it will automatically will be assigned to the category axis.
#' Otherwise to first value axis, unless you specify a different valueAxes for the guide.
#' 
#' @field legend
#' Object of class \code{"list"}.
#' List of an \code{\linkS4class{AmLegend}} class properties.
#' Properties of chart's legend.
#' 
#' @field segmentsField
#' Object of class \code{character}.
#' 
#' @field subChartProperties
#' Object of class \code{list}
#' 
#' @field theme
#' Object of class \code{character}.
#' Theme of a chart. Config files of themes can be found in amcharts/themes/ folder.
#' 
#' @field titles
#' Object of class \code{"list"}. List of Titles properties
#' See \code{\linkS4class{Title}} class.
#' 
#' @field trendLines
#' Object of class \code{"list"}.
#' List of \code{\linkS4class{TrendLine}} objects added to a chart.
#' You can add trend lines to a chart using this list or access already existing trend lines.
#' 
#' @field type
#' Object of class \code{"character"}.
#' Possible types are: serial, pie, radar,
#' (types xy, radar, funnel, gauge, map, stock. are in development).
#' 
#' @field valueAxes Object of class \code{"list"}. List of ValueAxes' properties.
#' See \code{\linkS4class{ValueAxis}} class.
#' Chart creates one value axis automatically,
#' so if you need only one value axis, you don't need to create it.
#' 
#' @field valueAxis
#' Object of class \code{list}.
#' List of Value axis properties for Gantt chart. Set it's type to "date" if your data is date or time based.
#' In case of Value axis for a Gantt chart. Set it's type to "date" if your data is date or time based.
#' 
#' @field listeners
#' Object of class \code{"list"} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string. See examples for details.
#' 
#' @field otherProperties
#' Object of class \code{"list"},
#' containing other avalaible properties non coded in the package yet.
#' 
#' @field value
#' Object of class \code{numeric}.
#' 
#' @field categoryAxesSettings
#' Object of class \code{"list"}.
#' List of a \code{\linkS4class{CategoryAxis}} properties.
#' CategoryAxesSettings settings set's settings common for all CategoryAxes of StockPanels.
#' If you change a property after the chart is initialized,
#' you should call stockChart.validateNow() method in order for it to work.
#' If there is no default value specified, default value of CategoryAxis class will be used.
#' you should get this axis from the chart and set properties to this object.
#' 
#' @field ChartCursorSettings
#' Object of class \code{"list"}.
#' List of a \code{\linkS4class{ChartCursor}} class properties.
#' ChartCursorSettings settings set's settings for chart cursor.
#' If you change a property after the chart is initialized,
#' you should call stockChart.validateNow() method in order for it to work.
#' If there is no default value specified, default value of ChartCursor class will be used.
#' 
#' @field ChartScrollbarSettings
#' Object of class \code{"list"}.
#' List of a \code{\linkS4class{ChartScrollbar}} class properties.
#' ChartScrollbarSettings settings set's settings for chart scrollbar.
#' If you change a property after the chart is initialized,
#' you should call stockChart.validateNow() method in order for it to work.
#' If there is no default value specified, default value of ChartScrollbar class will be used.
#' 
#' @field comparedDataSets
#' Object of class \code{"list"}. Properties of data sets selected for comparing.
#' 
#' @field dataSets
#' Object of class \code{"list"}.
#' Each element must a list of DataSet properties. 
#' 
#' @field dataSetSelector
#' Object of class \code{"list"}. DataSetSelector properties.
#' You can add it if you have more than one data set and want users to be able to select/compare them.
#' 
#' @field legendSettings
#' Object of class \code{"list"}. Legend settings.
#' 
#' @field mainDataSet
#' Object of class \code{"list"}. Data set selected as main.
#' 
#' @field panels: Object of class \code{"list"}.
#' Each element must be a list stockPanel properties.
#' 
#' @field panelsSettings
#' Object of class \code{"list"}. Settings for stock panels.
#' 
#' @field periodSelector
#' Object of class \code{"list"}.
#' Period selector object.
#' You can add it if you want user's to be able to enter
#' date ranges or zoom chart with predefined period buttons.
#' 
#' @field scrollbarChart
#' Read-only. Scrollbar's chart object.
#' 
#' @field stockEventsSettings
#' Object of class \code{"list"}. Settings for stock events.
#' 
#' @field valueAxesSettings
#' Object of class \code{"list"}. Settings for value axes.
#' 
#' @seealso \url{http://docs.amcharts.com/3/javascriptstockchart/AmStockChart}
#' 
#' @export
setClass("AmStockChart", contains = "AmChart",
         representation = representation(
           categoryAxesSettings = "list",
           chartCursorSettings = "list",
           chartScrollbarSettings = "list",
           comparedDataSets = "list",
           dataSets = "list",
           dataSetSelector = "list",
           legendSettings = "list",
           mainDataSet = "list",
           panels = "list",
           panelsSettings = "list",
           periodSelector = "list",
           scrollbarChart = "list",
           stockEventsSettings = "list",
           valueAxesSettings = "list"
         ),
         validity = function(object)
         {
           if (object@type != "stock") {
             stop("[AmStockChart]: you cannot change the type when creating AmStockChart")
           }
         }
)

#' @title Initialize an AmStockChart
#' @description Method for initializing any S4 class provided by the package.
#' @param .Object \linkS4class{AmStockChart}.
#' @param categoryAxesSettings \code{list} of \linkS4class{CategoryAxis}.
#' CategoryAxesSettings settings set's settings common for all CategoryAxes of StockPanels.
#' If you change a property after the chart is initialized,
#' you should call stockChart.validateNow() method in order for it to work.
#' If there is no default value specified, default value of CategoryAxis class will be used.
#' you should get this axis from the chart and set properties to this object.
#' @param chartCursorSettings \code{list} of \linkS4class{ChartCursor}.
#' ChartCursorSettings settings set's settings for chart cursor.
#' If you change a property after the chart is initialized,
#' you should call stockChart.validateNow() method in order for it to work.
#' If there is no default value specified, default value of ChartCursor class will be used.
#' @param chartScrollbarSettings \code{list} of \linkS4class{ChartScrollbar}.
#' ChartScrollbarSettings settings set's settings for chart scrollbar.
#' If you change a property after the chart is initialized,
#' you should call stockChart.validateNow() method in order for it to work.
#' If there is no default value specified, default value of ChartScrollbar class will be used.
#' @param comparedDataSets \linkS4class{DataSet}. Data sets selected for comparing.
#' @param dataSets \code{list} of \linkS4class{DataSet}. 
#' @param dataSetSelector \code{"list"}. DataSetSelector properties.
#' You can add it if you have more than one data set and want users to be able to select/compare them.
#' @param legendSettings \code{list}. Legend settings.
#' @param mainDataSet \code{list}. Data set selected as main.
#' @param panels \code{list} of \linkS4class{StockPanel}.
#' @param panelsSettings \code{list}. Settings for stock panels.
#' @param periodSelector \linkS4class{PeriodSelector}.
#' You can add it if you want user's to be able to enter
#' date ranges or zoom chart with predefined period buttons.
#' @param stockEventsSettings \code{list}. Settings for stock events.
#' @param theme \code{character}
#' @param valueAxesSettings \code{list}. Settings for value axes.
#' @param pathToImages \code{character}
#' @param ... Other properties...
#' @return An object of class \linkS4class{AmStockChart}.
#' @examples
#' new("AmStockChart", theme = "dark")
#' @rdname initialize-AmStockChart
#' @export
setMethod(f = "initialize", signature = "AmStockChart",
          definition = function(.Object, categoryAxesSettings, chartCursorSettings,
                                chartScrollbarSettings, comparedDataSets, dataSets,
                                dataSetSelector, legendSettings, mainDataSet, panels,
                                panelsSettings, periodSelector, stockEventsSettings,
                                theme, valueAxesSettings,
                                pathToImages = "http://www.amcharts.com/lib/3/images/",
                                ...)
          {
            .Object@type = "stock"
            if (!missing(categoryAxesSettings)) {
              .Object <- setCategoryAxesSettings(.Object, categoryAxesSettings)
            } else {}
            if (!missing(chartCursorSettings)) {
              .Object <- setChartCursorSettings(.Object, chartCursorSettings)
            } else {}
            if (!missing(chartScrollbarSettings)) {
              .Object <- setChartScrollbarSettings(.Object, chartScrollbarSettings)
            } else {}
            if (!missing(comparedDataSets)) {
              .Object <- setComparedDataSets(.Object, comparedDataSets)
            } else {}
            if (!missing(dataSets)) {
              .Object <- setDataSets(.Object, dataSets)
            } else {}
            if (!missing(dataSetSelector)) {
              .Object <- setDataSetSelector(.Object, dataSetSelector)
            } else {}
            if (!missing(legendSettings)) {
              .Object <- setLegendSettings(.Object, legendSettings)
            } else {}
            if (!missing(mainDataSet)) {
              .Object <- setMainDataSet(.Object, mainDataSet)
            } else {}
            if (!missing(panels)) {
              .Object <- setPanels(.Object, panels)
            } else {}
            if (!missing(panelsSettings)) {
              .Object <- setPanelsSettings(.Object, panelsSettings)
            } else {}
            if (!missing(periodSelector)) {
              .Object <- setPeriodSelector(.Object, periodSelector)
            } else {}
            if (!missing(stockEventsSettings)) {
              .Object <- setStockEventsSettings(.Object, stockEventsSettings)
            } else {}
            if (!missing(theme)) {
              .Object@theme <- theme
            } else {}
            if (!missing(valueAxesSettings)) {
              .Object <- setValueAxesSettings(.Object, valueAxesSettings)
            } else {}
            .Object <- setProperties(.Object, pathToImages = pathToImages, ...)
            validObject(.Object)
            return(.Object)
          })

#' @description amStockChart is a shortcut constructor 
#' for instantiating AmChart of type \code{stock}
#' @examples
#' amStockChart()
#' @describeIn initialize-AmStockChart
#' @export
amStockChart <- function(categoryAxesSettings, chartCursorSettings, chartScrollbarSettings,
                         comparedDataSets, dataSets, dataSetSelector, legendSettings,
                         mainDataSet, panels, panelsSettings, periodSelector,
                         stockEventsSettings, theme, valueAxesSettings, ...)
{
  .Object = new("AmStockChart")
  if (!missing(categoryAxesSettings)) {
    .Object <- setCategoryAxesSettings(.Object, categoryAxesSettings)
  } else {}
  if (!missing(chartCursorSettings)) {
    .Object <- setChartCursorSettings(.Object, chartCursorSettings)
  } else {}
  if (!missing(chartScrollbarSettings)) {
    .Object <- setChartScrollbarSettings(.Object, chartScrollbarSettings)
  } else {}
  if (!missing(comparedDataSets)) {
    .Object <- setComparedDataSets(.Object, comparedDataSets)
  } else {}
  if (!missing(dataSets)) {
    .Object <- setDataSets(.Object, dataSets)
  } else {}
  if (!missing(dataSetSelector)) {
    .Object <- setDataSetSelector(.Object, dataSetSelector)
  } else {}
  if (!missing(legendSettings)) {
    .Object <- setLegendSettings(.Object, legendSettings)
  } else {}
  if (!missing(mainDataSet)) {
    .Object <- setMainDataSet(.Object, mainDataSet)
  } else {}
  if (!missing(panels)) {
    .Object <- setPanels(.Object, panels)
  } else {}
  if (!missing(panelsSettings)) {
    .Object <- setPanelsSettings(.Object, panelsSettings)
  } else {}
  if (!missing(periodSelector)) {
    .Object <- setPeriodSelector(.Object, periodSelector)
  } else {}
  if (!missing(stockEventsSettings)) {
    .Object <- setStockEventsSettings(.Object, stockEventsSettings)
  } else {}
  if (!missing(theme)) {
    .Object@theme <- theme
  } else {}
  if (!missing(valueAxesSettings)) {
    .Object <- setValueAxesSettings(.Object, valueAxesSettings)
  } else {}
  .Object <- setProperties(.Object,...)
  validObject(.Object)
  return(.Object)
}

# > @categoryAxesSettings: setters ####

#' @examples
#' setCategoryAxesSettings(.Object = amStockChart(), gridPosition = "start")
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{CategoryAxis}} S4 class
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setCategoryAxesSettings",
           def = function(.Object, categoryAxesSettings = NULL, ...) {standardGeneric("setCategoryAxesSettings")})
#' @rdname initialize-AmStockChart
setMethod(f = "setCategoryAxesSettings", signature = c("AmStockChart"),
          definition = function(.Object, categoryAxesSettings = NULL, ...)
          {
            if (is.null(categoryAxesSettings)) {
              categoryAxesSettings <- categoryAxis(...)
            } else {}
            .Object@categoryAxesSettings <- listProperties(categoryAxesSettings)
            validObject(.Object)
            return(.Object)
          })

# > @chartCursorSettings : setters ####

#' @examples
#' setChartCursorSettings(.Object = amStockChart(), oneBallOnly = TRUE)
#' setChartCursorSettings(.Object = amStockChart(), chartCursorSettings = chartCursor())
#' \dontrun{
#' setChartCursorSettings(.Object = amStockChart(), chartCursorSettings = "foo")
#' }
#' @seealso \code{\linkS4class{ChartCursor}} S4 class
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setChartCursorSettings",
           def = function(.Object, chartCursorSettings = NULL, ...) {standardGeneric("setChartCursorSettings")})
#' @rdname initialize-AmStockChart
setMethod(f = "setChartCursorSettings", signature = c("AmStockChart"),
          definition = function(.Object, chartCursorSettings = NULL, ...)
          {
            if (is.null(chartCursorSettings)) {
              chartCursorSettings <- chartCursor(...)
            } else {}
            .Object@chartCursorSettings <- listProperties(chartCursorSettings)
            validObject(.Object)
            return(.Object)
          })

# > @chartScrollbarSettings : setters ####

#' @examples
#' setChartScrollbarSettings(.Object = amStockChart(), chartScrollbarSettings = chartScrollbar())
#' @seealso \code{\linkS4class{ChartScrollbar}} S4 class
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setChartScrollbarSettings",
           def = function(.Object, chartScrollbarSettings = NULL, ...) {standardGeneric("setChartScrollbarSettings")})
#' @rdname initialize-AmStockChart
setMethod(f = "setChartScrollbarSettings", signature = c("AmStockChart"),
          definition = function(.Object, chartScrollbarSettings = NULL, ...)
          {
            if (is.null(chartScrollbarSettings)) {
              chartScrollbarSettings <- chartScrollbar(...)
            } else {}
            .Object@chartScrollbarSettings <- listProperties(chartScrollbarSettings)
            validObject(.Object)
            return(.Object)
          })

# > @comparedDataSets : setters ####

#' @examples
#' library(pipeR)
#' amStockChart() %>>% setComparedDataSets(list(dataSet(compared = TRUE), dataSet(compared = TRUE)))
#' @seealso \code{\linkS4class{DataSet}} S4 class
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setComparedDataSets",
           def = function(.Object, comparedDataSets) {standardGeneric("setComparedDataSets")})
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

#' @examples
#' addComparedDataSet(.Object = amStockChart(), compared = TRUE)
#' @seealso \code{\linkS4class{DataSet}} S4 class
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "addComparedDataSet",
           def = function(.Object, dataSet = NULL, ...) {standardGeneric("addComparedDataSet")})
#' @rdname initialize-AmStockChart
setMethod(f = "addComparedDataSet", signature = c("AmStockChart"),
          definition = function(.Object, dataSet = NULL, ...)
          {
            if (is.null(dataSet) && !missing(...)) {
              dataSet <- dataSet(...)
            } else {}
            .Object@comparedDataSets <- rlist::list.append(.Object@comparedDataSets,
                                                           listProperties(dataSet))
            validObject(.Object)
            return(.Object)
          })

# > @dataSets : setters ####

#' @examples
#' library(pipeR)
#' amStockChart() %>>% setDataSets(list(dataSet(compared = FALSE), dataSet(compared = FALSE)))
#' @seealso \code{\linkS4class{DataSet}} S4 class
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setDataSets",
           def = function(.Object, dataSets) {standardGeneric("setDataSets")})
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

#' @param dataSet \linkS4class{DataSet}.
#' @examples
#' library(pipeR)
#' amStockChart() %>>% addDataSet(compared = FALSE) %>>% addDataSet(dataSet())
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "addDataSet",
           def = function(.Object, dataSet = NULL, ...) {standardGeneric("addDataSet")})
#' @rdname initialize-AmStockChart
setMethod(f = "addDataSet", signature = c("AmStockChart"),
          definition = function(.Object, dataSet = NULL, ...)
          {
            if (is.null(dataSet) && !missing(...)) {
              dataSet <- dataSet(...)
            } else {}
            .Object@dataSets <- rlist::list.append(.Object@dataSets,
                                                   listProperties(dataSet))
            validObject(.Object)
            return(.Object)
          })

# > @dataSetSelector : setters ####

#' @examples
#' setDataSetSelector(.Object = amStockChart(), width = 180)
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setDataSetSelector",
           def = function(.Object, ...) {standardGeneric("setDataSetSelector")})
#' @rdname initialize-AmStockChart
setMethod(f = "setDataSetSelector", signature = c("AmStockChart"),
          definition = function(.Object, ...)
          {
            .Object@dataSetSelector <- list(...)
            validObject(.Object)
            return(.Object)
          })

# > @legendSettings : setters ####

#' @examples
#' setLegendSettings(.Object = amStockChart(), equalWidths = TRUE)
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setLegendSettings", def = function(.Object, ...) {standardGeneric("setLegendSettings")})
#' @rdname initialize-AmStockChart
setMethod(f = "setLegendSettings", signature = c("AmStockChart"),
          definition = function(.Object, ...)
          {
            .Object@legendSettings <- list(...)
            validObject(.Object)
            return(.Object)
          })

# > @mainDataSet: setter ###
#' @examples
#' setMainDataSet(.Object = amStockChart(), showInCompare = TRUE)
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setMainDataSet",
           def = function(.Object, dataSet = NULL, ...) {standardGeneric("setMainDataSet")})
#' @rdname initialize-AmStockChart
setMethod(f = "setMainDataSet", signature = c("AmStockChart"),
          definition = function(.Object, dataSet = NULL, ...)
          {
            if (is.null(dataSet) && !missing(...)) {
              dataSet <- dataSet(...)
            } else {}
            .Object@mainDataSet <- listProperties(dataSet)
            validObject(.Object)
            return(.Object)
          })

# > @panels : setters ####

#' @examples
#' panels <- list(stockPanel(compared = TRUE), stockPanel(compared = TRUE))
#' setPanels(.Object = amStockChart(), panels = panels)
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setPanels",
           def = function(.Object, panels) {standardGeneric("setPanels")})
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
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "addPanel",
           def = function(.Object, panel = NULL, ...) {standardGeneric("addPanel")})
#' @rdname initialize-AmStockChart
setMethod(f = "addPanel", signature = c("AmStockChart"),
          definition = function(.Object, panel = NULL, ...)
          {
            if (is.null(panel) && !missing(...)) {
              panel <- stockPanel(...)
            } else {}
            .Object@panels <- rlist::list.append(.Object@panels, listProperties(panel))
            validObject(.Object)
            return(.Object)
          })

# > @panelSettings : setters ####

#' @description Setter for PanelsSettings.
#' @examples
#' setPanelsSettings(.Object = amStockChart(), backgroundAlpha = 0)
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setPanelsSettings", def = function(.Object, ...) {standardGeneric("setPanelsSettings")})
#' @rdname initialize-AmStockChart
setMethod(f = "setPanelsSettings", signature = c("AmStockChart"),
          definition = function(.Object, ...)
          {
            .Object@panelsSettings <- list(...)
            validObject(.Object)
            return(.Object)
          })

# > @setPeriodSelector : setters ####

#' @examples
#' setPeriodSelector(.Object = amStockChart(), dateFormat = "DD-MM-YYYY")
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setPeriodSelector", def = function(.Object, periodSelector = NULL, ...) {standardGeneric("setPeriodSelector")})
#' @rdname initialize-AmStockChart
setMethod(f = "setPeriodSelector", signature = c("AmStockChart"),
          definition = function(.Object, periodSelector = NULL, ...)
          {
            if (is.null(periodSelector) && !missing(...)) {
              periodSelector <- periodSelector(...)
            } else {}
            .Object@periodSelector <- listProperties(periodSelector)
            validObject(.Object)
            return(.Object)
          })

# > @setStockEventsSettings : setters ####

#' @examples
#' setStockEventsSettings(.Object = amStockChart(), backgroundAlpha = 1)
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setStockEventsSettings", def = function(.Object, ...) {standardGeneric("setStockEventsSettings")})
#' @rdname initialize-AmStockChart
setMethod(f = "setStockEventsSettings", signature = c("AmStockChart"),
          definition = function(.Object, ...)
          {
            .Object@stockEventsSettings <- list(...)
            validObject(.Object)
            return(.Object)
          })

# > @valueAxesSettings: setters ####

#' @examples
#' setValueAxesSettings(.Object = amStockChart(), autoGridCount = "TRUE")
#' @rdname initialize-AmStockChart
#' @export
setGeneric(name = "setValueAxesSettings",
           def = function(.Object, valueAxesSettings = NULL, ...) {standardGeneric("setValueAxesSettings")})
#' @rdname initialize-AmStockChart
setMethod(f = "setValueAxesSettings", signature = c("AmStockChart"),
          definition = function(.Object, ...)
          {
            .Object@valueAxesSettings <- list(...)
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' listProperties(amChart(test = 1))
#' @rdname listProperties-AmObject
setMethod(f = "listProperties", signature = "AmStockChart",
          definition = function(.Object)
          {
            ls <- callNextMethod()
            if (length(.Object@categoryAxesSettings)) {
              ls <- rlist::list.append(ls, categoryAxesSettings = .Object@categoryAxesSettings)
            } else {}
            if (length(.Object@chartCursorSettings)) {
              ls <- rlist::list.append(ls, chartCursorSettings = .Object@chartCursorSettings)
            } else {}
            if (length(.Object@chartScrollbarSettings)) {
              ls <- rlist::list.append(ls, chartScrollbarSettings = .Object@chartScrollbarSettings)
            } else {}
            if (length(.Object@comparedDataSets)) {
              ls <- rlist::list.append(ls, comparedDataSets = .Object@comparedDataSets)
            } else {}
            if (length(.Object@dataSets)) {
              ls <- rlist::list.append(ls, dataSets = .Object@dataSets)
            } else {}
            if (length(.Object@dataSetSelector)) {
              ls <- rlist::list.append(ls, dataSetSelector = .Object@dataSetSelector)
            } else {}
            if (length(.Object@legendSettings)) {
              ls <- rlist::list.append(ls, legendSettings = .Object@legendSettings)
            } else {}
            if (length(.Object@mainDataSet)) {
              ls <- rlist::list.append(ls, mainDataSet = .Object@mainDataSet)
            } else {}
            if (length(.Object@panels)) {
              ls <- rlist::list.append(ls, panels = .Object@panels)
            } else {}
            if (length(.Object@panelsSettings)) {
              ls <- rlist::list.append(ls, panelsSettings = .Object@panelsSettings)
            } else {}
            if (length(.Object@periodSelector)) {
              ls <- rlist::list.append(ls, panelsSettings = .Object@periodSelector)
            } else {}
            if (length(.Object@periodSelector)) {
              ls <- rlist::list.append(ls, periodSelector = .Object@periodSelector)
            } else {}
            if (length(.Object@stockEventsSettings)) {
              ls <- rlist::list.append(ls, stockEventsSettings = .Object@stockEventsSettings)
            } else {}
            if (length(.Object@valueAxesSettings)) {
              ls <- rlist::list.append(ls, valueAxesSettings = .Object@valueAxesSettings)
            } else {}
            return(ls)
          })