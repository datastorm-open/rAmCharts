#' @include AmObject.R
NULL

#' @title AmStockChart
#' @author Dataknowledge
#' @description Class to draw stock charts
#' 
#' @slot balloon \linkS4class{AmBalloon}.
#' @slot comparedDataSets \code{list} of \linkS4class{DataSet}.
#' Properties of data sets selected for comparing.
#' @slot dataSets \code{list} of \linkS4class{DataSet}.
#' Each element must a list of DataSet properties.
#' @slot dataSetSelector \code{list} of \linkS4class{DataSetSelector}.
#' You can add it if you have more than one data set and want users
#' to be able to select/compare them.
#' @slot mainDataSet \linkS4class{DataSet}.
#' Data set selected as main.
#' @slot panels \code{list} of \linkS4class{StockPanel}.
#' @slot periodSelector \linkS4class{PeriodSelector}.
#' You can add it if you want user's to be able to enter
#' date ranges or zoom chart with predefined period buttons.
#' @slot theme \code{character}
#' @slot type equals "stock"
#' 
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string. See examples for details.
#' @slot otherProperties \code{list},
#' containing other avalaible properties non coded in the class.
#' @slot value \code{numeric}.
#' 
#' @seealso \url{http://docs.amcharts.com/3/javascriptstockchart/AmStockChart}
#' @export
setClass("AmStockChart", contains = "AmObject",
         representation = representation(
           balloon = "list",
           comparedDataSets = "list",
           dataSets = "list",
           dataSetSelector = "list",
           mainDataSet = "list",
           panels = "list",
           periodSelector = "list",
           theme = "character",
           type = "character"
         ),
         validity = function(object)
         {
           if (object@type != "stock") {
             stop("[AmStockChart]: you cannot change the type when creating AmStockChart")
           }
           if (length(object@theme) > 1) {
             stop("[AmStockChart]: invalid theme")
           }
         })

#' @title Initialize an AmStockChart
#' @description Method for initializing any S4 class provided by the package.
#' @param .Object \linkS4class{AmStockChart}.
#' @param balloon \linkS4class{AmBalloon}.
#' @param comparedDataSets \code{list} of \linkS4class{DataSet}.
#' Properties of data sets selected for comparing.
#' @param dataSets \code{list} of \linkS4class{DataSet}.
#' Each element must a list of DataSet properties.
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
#' @param ... Other properties...
#' @return An object of class \linkS4class{AmStockChart}.
#' @examples
#' new("AmStockChart", theme = "dark")
#' @rdname initialize-AmStockChart
#' @export
setMethod(f = "initialize", signature = "AmStockChart",
          definition = function(.Object, balloon, comparedDataSets, dataSets,
                                dataSetSelector, mainDataSet,
                                panels, periodSelector, theme, ...)
          {
            .Object@type = "stock"
            if (!missing(balloon)) {
              .Object <- setBalloon(.Object = .Object, amBalloon = balloon)
            } else {}
            if (!missing(comparedDataSets)) {
              .Object <- setComparedDataSets(.Object = .Object, comparedDataSets = comparedDataSets)
            } else {}
            if (!missing(dataSets)) {
              .Object <- setDataSets(.Object = .Object, dataSets = dataSets)
            } else {}
            if (!missing(dataSetSelector)) {
              .Object <- setDataSetSelector(.Object = .Object, dataSetSelector = dataSetSelector)
            } else {}
            if (!missing(mainDataSet)) {
              .Object <- setMainDataSet(.Object = .Object, mainDataSet = mainDataSet)
            } else {}
            if (!missing(panels)) {
              .Object <- setPanels(.Object = .Object, panels = panels)
            } else {}
            if (!missing(periodSelector)) {
              .Object <- setPeriodSelector(.Object = .Object, periodSelector = periodSelector)
            } else {}
            if (!missing(theme)) {
              .Object@theme <- theme
            } else {}
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

#' @description amStockChart is a shortcut constructor 
#' for instantiating AmChart of type \code{stock}
#' @examples
#' amStockChart()
#' # ---
#' @rdname initialize-AmStockChart
#' @export
amStockChart <- function(balloon, comparedDataSets, dataSets,
                         dataSetSelector, mainDataSet,
                         panels, periodSelector, theme, ...)
{
  .Object = new("AmStockChart", type = "stock")
  if (!missing(balloon)) {
    .Object <- setBalloon(.Object = .Object, amBalloon = balloon)
  } else {}
  if (!missing(comparedDataSets)) {
    .Object <- setComparedDataSets(.Object = .Object, comparedDataSets = comparedDataSets)
  } else {}
  if (!missing(dataSets)) {
    .Object <- setDataSets(.Object = .Object, dataSets = dataSets)
  } else {}
  if (!missing(dataSetSelector)) {
    .Object <- setDataSetSelector(.Object = .Object, dataSetSelector = dataSetSelector)
  } else {}
  if (!missing(mainDataSet)) {
    .Object <- setMainDataSet(.Object = .Object, mainDataSet = mainDataSet)
  } else {}
  if (!missing(panels)) {
    .Object <- setPanels(.Object = .Object, panels = panels)
  } else {}
  if (!missing(periodSelector)) {
    .Object <- setPeriodSelector(.Object = .Object, periodSelector = periodSelector)
  } else {}
  if (!missing(theme)) {
    .Object@theme <- theme
  } else {}
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return(.Object)
}

#' @examples
#' listProperties(amStockChart(test = 1))
#' # ---
#' @rdname listProperties-AmObject
setMethod(f = "listProperties", signature = "AmStockChart",
          definition = function(.Object)
          {
            ls <- callNextMethod()
            if (length(.Object@balloon)) {
              ls <- rlist::list.append(ls, balloon = .Object@balloon)
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
            if (length(.Object@mainDataSet)) {
              ls <- rlist::list.append(ls, mainDataSet = .Object@mainDataSet)
            } else {}
            if (length(.Object@panels)) {
              ls <- rlist::list.append(ls, panels = .Object@panels)
            } else {}
            if (length(.Object@periodSelector)) {
              ls <- rlist::list.append(ls, periodSelector = .Object@periodSelector)
            } else {}
            if (length(.Object@type)) {
              ls <- rlist::list.append(ls, type = .Object@type)
            } else {}
            if (length(.Object@theme)) {
              ls <- rlist::list.append(ls, theme = .Object@theme)
            } else {}
            return(ls)
          })

#' @title Visualize AmStockChart with show
#' @param object \linkS4class{AmStockChart}
#' @family Visualizations
#' @export
setMethod(f = "show", signature = "AmStockChart",
          definition = function(object)
          {
            if (length(object@type)) {
              print(plot(object))
            } else {
              print(object)
            }
          })