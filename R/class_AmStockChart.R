#' @include class_AmObject.R
NULL

#' @title AmStockChart
#' @author datastorm-open
#' @description Class to draw stock charts
#' 
#' @slot balloon \linkS4class{AmBalloon}.
#' @slot comparedDataSets \code{list} of \linkS4class{DataSet}.
#' Properties of data sets selected for comparison.
#' @slot dataSets \code{list} of \linkS4class{DataSet}.
#' Each element must be a list of DataSet properties.
#' @slot dataSetSelector \code{list} of \linkS4class{DataSetSelector}.
#' You can add it if you have more than one data set and want users
#' to be able to select/compare them.
#' @slot mainDataSet \linkS4class{DataSet}.
#' Data set selected as main.
#' @slot panels \code{list} of \linkS4class{StockPanel}.
#' @slot periodSelector \linkS4class{PeriodSelector}.
#' You can add it if you want users to be able to enter
#' date ranges or zoom chart with predefined period buttons.
#' @slot theme \code{character}
#' @slot type equals "stock"
#' @slot group \code{character} for synchronization
#' @slot is_ts_module \code{logicalOrMissing}. Don't use. For \link{rAmChartsTimeSeriesUI}
#' 
#' @slot listeners \code{list} containining the listeners to add to the chart.
#' The list must be named as in the official API. Each element must be a character string.
#' @slot otherProperties \code{list}
#' containing other avalaible properties not yet implemented in the package.
#' @slot value \code{numeric}.
#' 
#' @seealso \url{http://docs.amcharts.com/3/javascriptstockchart/AmStockChart}
#' 
#' 
setClass("AmStockChart", contains = "AmObject",
         representation = representation(balloon = "list",
                                         comparedDataSets = "list",
                                         dataSets = "list",
                                         dataSetSelector = "list",
                                         mainDataSet = "list",
                                         panels = "list",
                                         periodSelector = "list",
                                         theme = "character",
                                         type = "character",
                                         group = "characterOrNULL",
                                         is_ts_module = "logicalOrMissing"),
         validity = function(object)
         {
           if (object@type != "stock") {
             stop("[AmStockChart]: you cannot change the type when creating AmStockChart")
           }
           if (length(object@theme) > 1) {
             stop("[AmStockChart]: invalid theme")
           }
         })

#' @title Initializes an AmStockChart
#' @description Method to initialize any S4 class provided by the package.
#' 
#' @param .Object \linkS4class{AmStockChart}.
#' @param balloon \linkS4class{AmBalloon}.
#' @param comparedDataSets \code{list} of \linkS4class{DataSet}.
#' Properties of data sets selected for comparing.
#' @param dataSets \code{list} of \linkS4class{DataSet}.
#' Each element must be a list of DataSet properties.
#' @param dataSetSelector \code{list} of \linkS4class{DataSetSelector}.
#' You can add it if you have more than one data set and want users
#' to be able to select/compare them.
#' @param mainDataSet \linkS4class{DataSet}.
#' Data set selected as main.
#' @param panels \code{list} of \linkS4class{StockPanel}.
#' @param periodSelector \linkS4class{PeriodSelector}.
#' You can add it if you want user's to be able to enter
#' date ranges or zoom chart with predefined period buttons.
#' @param theme \code{character}.
#' @param group \code{character} for synchronization
#' @param is_ts_module \code{boolean}. Don't use. For \link{rAmChartsTimeSeriesUI}
#' 
#' @param ... other properties of AmStockChart.
#' 
#' @return An object of class \linkS4class{AmStockChart}.
#' 
#' @examples
#' \donttest{
#' # --- method 'initialize'
#' new("AmStockChart", theme = "dark")
#' }
#' 
#' @rdname AmStockChart
#' 
#' @export
#' 
setMethod(f = "initialize", signature = "AmStockChart",
          definition = function(.Object, balloon, comparedDataSets, dataSets,
                                dataSetSelector, mainDataSet,
                                panels, periodSelector, theme, group, is_ts_module, ...)
          {
            .Object@type = "stock"
            if (!missing(balloon))
              .Object <- setBalloon(.Object = .Object, amBalloon = balloon)
            if (!missing(comparedDataSets))
              .Object <- setComparedDataSets(.Object = .Object, comparedDataSets = comparedDataSets)
            if (!missing(dataSets))
              .Object <- setDataSets(.Object = .Object, dataSets = dataSets)
            if (!missing(dataSetSelector))
              .Object <- setDataSetSelector(.Object = .Object, dataSetSelector = dataSetSelector)
            if (!missing(mainDataSet))
              .Object <- setMainDataSet(.Object = .Object, mainDataSet = mainDataSet)
            if (!missing(panels))
              .Object <- setPanels(.Object = .Object, panels = panels)
            if (!missing(periodSelector))
              .Object <- setPeriodSelector(.Object = .Object, periodSelector = periodSelector)
            if (!missing(theme))
              .Object@theme <- theme
            if (!missing(group))
              .Object@group <- group
            if (!missing(is_ts_module))
              .Object@is_ts_module <- is_ts_module
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

#' @rdname AmStockChart
#' @examples
#' \donttest{
#' # --- constructor
#' amStockChart()
#' }
#' @export
#' 
amStockChart <- function(balloon, comparedDataSets, dataSets,
                         dataSetSelector, mainDataSet,
                         panels, periodSelector, theme, group, is_ts_module, ...)
{
  .Object <- new("AmStockChart")
  if (!missing(balloon))
    .Object <- setBalloon(.Object = .Object, amBalloon = balloon)
  if (!missing(comparedDataSets))
    .Object <- setComparedDataSets(.Object = .Object, comparedDataSets = comparedDataSets)
  if (!missing(dataSets))
    .Object <- setDataSets(.Object = .Object, dataSets = dataSets)
  if (!missing(dataSetSelector))
    .Object <- setDataSetSelector(.Object = .Object, dataSetSelector = dataSetSelector)
  if (!missing(mainDataSet))
    .Object <- setMainDataSet(.Object = .Object, mainDataSet = mainDataSet)
  if (!missing(panels))
    .Object <- setPanels(.Object = .Object, panels = panels)
  if (!missing(periodSelector))
    .Object <- setPeriodSelector(.Object = .Object, periodSelector = periodSelector)
  if (!missing(theme))
    .Object@theme <- theme
  if (!missing(group))
    .Object@group <- group
  if (!missing(is_ts_module))
    .Object@is_ts_module <- is_ts_module
  .Object <- setProperties(.Object, ...)
  
  validObject(.Object)
  return(.Object)
}

#' #' @rdname listProperties-AmObject
#' #' @examples
#' #' # --- signature 'AmStockChart'
#' #' listProperties(amStockChart(test = 1))
#' #' 
#' setMethod(f = "listProperties", signature = "AmStockChart",
#'           definition = function(.Object)
#'           {
#'             ls <- callNextMethod()
#'             if (length(.Object@balloon))
#'               ls <- rlist::list.append(ls, balloon = .Object@balloon)
#'             if (length(.Object@comparedDataSets))
#'               ls <- rlist::list.append(ls, comparedDataSets = .Object@comparedDataSets)
#'             if (length(.Object@dataSets))
#'               ls <- rlist::list.append(ls, dataSets = .Object@dataSets)
#'             if (length(.Object@dataSetSelector))
#'               ls <- rlist::list.append(ls, dataSetSelector = .Object@dataSetSelector)
#'             if (length(.Object@mainDataSet))
#'               ls <- rlist::list.append(ls, mainDataSet = .Object@mainDataSet)
#'             if (length(.Object@panels))
#'               ls <- rlist::list.append(ls, panels = .Object@panels)
#'             if (length(.Object@periodSelector))
#'               ls <- rlist::list.append(ls, periodSelector = .Object@periodSelector)
#'             if (length(.Object@type))
#'               ls <- rlist::list.append(ls, type = .Object@type)
#'             if (length(.Object@theme))
#'               ls <- rlist::list.append(ls, theme = .Object@theme)
#'             
#'             return(ls)
#'           })