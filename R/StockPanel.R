#' @include AmObject.R AmGraph.R AmLegend.R ValueAxis.R
NULL

#' @title StockPanel class
#' 
#' @field drawOnAxis \code{list}. Containing properties of ValueAxis.
#' Specifies on which value axis user can draw trend lines.
#' Set drawingIconsEnabled to true if you want drawing icons to be visible.
#' First value axis will be used if not set here.
#' You can use a reference to the value axis object or id of value axis.
#' 
#' @field stockGraphs \code{list}.
#' Each element must be have been created with stockGraph(*)
#' 
#' @field stockLegend \code{list}.
#' Each element must be have been created with stockLegend(*)
#' 
#' @field allLabels \code{list} of \linkS4class{Label} properties.
#' 
#' @field arrows \code{list} of \linkS4class{GaugeArrow} properties.
#' 
#' @field axes \code{list} of \linkS4class{GaugeAxis} properties.
#' 
#' @field balloon \code{list} of an \linkS4class{AmBalloon} properties.
#' Creates the balloons (tooltips) of the chart,
#' It follows the mouse cursor when you roll-over the data items.
#' The framework generates the instances automatically you only need to adjust
#' the appearance to your needs.
#' 
#' @field categoryAxis \code{list} of a \linkS4class{CategoryAxis} properties.
#' Read-only. Chart creates category axis itself.
#' If you want to change some properties,
#' you should get this axis from the chart and set properties to this object.
#' 
#' @field categoryField \code{character}.
#' Category field name tells the chart the name of the field in your dataProvider object
#' which will be used for category axis values.
#' 
#' @field ChartCursor \code{list} of a \linkS4class{ChartCursor} properties.
#' 
#' @field ChartScrollbar \code{list} of a \linkS4class{ChartScrollbar} properties.
#' 
#' @field creditsPosition \code{character},
#' specifying position of link to amCharts site.
#' Allowed values are: top-left, top-right, bottom-left and bottom-right.
#' 
#' @field dataProvider \code{list}, containing the data.
#' Use providing method toList* to convert a \code{data.frame}.
#' 
#' @field graphs \code{list}.  List of AmGraphs properties
#' See \linkS4class{AmGraph}.
#' Creates the visualization of the data in following types: line, column, step line,
#' smoothed line, olhc and candlestick.
#' 
#' @field graph \code{list} of \linkS4class{AmGraph} properties.
#' Graph of a Gantt chart. Gant chart actually creates multiple graphs (separate for each segment).
#' Properties of this graph are passed to each of the created graphs
#' - this allows you to control the look of segments.
#' 
#' @field guides \code{list} of Guides properties.
#' See \linkS4class{Guide}.
#' Instead of adding guides to the axes, you can push all of them to this array.
#' In case guide has category or date defined, it will automatically will be assigned to the category axis.
#' Otherwise to first value axis, unless you specify a different valueAxes for the guide.
#' 
#' @field legend \code{list} of an \linkS4class{AmLegend} properties.
#' 
#' @field segmentsField \code{character}.
#' 
#' @field subChartProperties \code{list}.
#' 
#' @field theme \code{character}.
#' Theme of a chart. Config files of themes can be found in amcharts/themes/ folder.
#' 
#' @field titles \code{list} of Titles properties.
#' See \linkS4class{Title}.
#' 
#' @field trendLines \code{list} of \linkS4class{TrendLine}.
#' You can add trend lines to a chart using this list or access already existing trend lines.
#' 
#' @field type \code{character}.
#' Possible types are: serial, pie, radar,
#' (types xy, radar, funnel, gauge, map, stock. are in development).
#' 
#' @field valueAxes Object of class \code{list}. List of ValueAxes' properties.
#' See \linkS4class{ValueAxis}.
#' Chart creates one value axis automatically,
#' so if you need only one value axis, you don't need to create it.
#' 
#' @field valueAxis \code{list} of Value axis properties for Gantt chart.
#' Set it's type to "date" if your data is date or time based.
#' In case of Value axis for a Gantt chart. Set it's type to "date" if your data is date or time based.
#' 
#' @field listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string. See examples for details.
#' 
#' @field otherProperties \code{list},
#' containing other avalaible properties for the class.
#' 
#' @field value \code{numeric}.
#' 
#' @author DataKnowledge
#' @export
setClass(Class = "StockPanel", contains = "AmChart",
          representation = representation(
            drawOnAxis = "list",
            stockGraphs = "list",
            stockLegend = "list"
         ))

#' @title Initialize a StockPanel
#' @param .Object \linkS4class{StockPanel}.
#' @param drawOnAxis \linkS4class{ValueAxis}.
#' Specifies on which value axis user can draw trend lines.
#' Set drawingIconsEnabled to true if you want drawing icons to be visible.
#' First value axis will be used if not set here.
#' You can use a reference to the value axis object or id of value axis.
#' @param stockGraphs \code{list} of \linkS4class{AmGraph}.
#' Each element must be have been created with stockGraph(*)
#' @param stockLegend \code{list} of \linkS4class{AmLegend}.
#' Each element must be have been created with stockLegend(*)
#' @param ... Other properties...
#' @return (updated) \linkS4class{StockPanel} with given properties.
#' @examples
#' new("StockPanel")
#' @rdname initialize-StockPanel
#' @export
setMethod(f = "initialize", signature = "StockPanel",
          definition = function(.Object, drawOnAxis, stockGraphs, stockLegend, ...)
          {  
            if(!missing(drawOnAxis)){
              .Object <- setDrawOnAxis(.Object, drawOnAxis)
            }
            if(!missing(stockGraphs)){
              .Object <- setStockGraphs(.Object, stockGraphs)
            }
            if(!missing(stockLegend)){
              .Object <- setStockLegend(.Object, stockLegend)
            }
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####

#' @describeIn initialize-StockPanel
#' @examples
#' stockPanel()
#' @export
stockPanel <- function(drawOnAxis, stockGraphs, stockLegend, ...){
  .Object <- new("StockPanel")
  if (!missing(drawOnAxis)) {
    .Object <- setDrawOnAxis(.Object, drawOnAxis)
  } else {}
  if (!missing(stockGraphs)) {
    .Object <- setStockGraphs(.Object, stockGraphs)
  } else {}
  if (!missing(stockLegend)) {
    .Object <- setStockLegend(.Object, stockLegend)
  } else {}
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return(.Object)
}

# > @drawOnAxis : setters ####

#' @param valueAxis \linkS4class{ValueAxis}
#' @examples
#' setDrawOnAxis(.Object = stockPanel(), valueAxis = valueAxis())
#' setDrawOnAxis(.Object = stockPanel(), valueAxis = "valueAxis1")
#' @rdname initialize-StockPanel
#' @export
setGeneric(name = "setDrawOnAxis",
           def = function(.Object, valueAxis = NULL, ...){ standardGeneric("setDrawOnAxis") })
#' @rdname initialize-StockPanel
setMethod(f = "setDrawOnAxis", signature = c("StockPanel"),
  definition = function(.Object, valueAxis = NULL, ...)
  {
    if (is.null(valueAxis) && !missing(...)) {
      valueAxis <- valueAxis(...)
    } else if (is(valueAxis, "ValueAxis")) {
      .Object@drawOnAxis<- listProperties(valueAxis)
    } else if (is.character(valueAxis)) {
      .Object <- setProperties(.Object, drawOnAxis = valueAxis)
    } else {}
    validObject(.Object)
    return(.Object)
  })

# > @stockGraphs : setters ####

#' @examples
#' stockGraphs <- list(stockGraph(comparable = TRUE), stockGraph(comparable = FALSE)) 
#' setStockGraphs(.Object =  stockPanel(), stockGraphs = stockGraphs)
#' stockPanel(stockGraphs = stockGraphs)
#' @rdname initialize-StockPanel
#' @export
setGeneric(name = "setStockGraphs",
           def = function(.Object, stockGraphs){ standardGeneric("setStockGraphs") })
#' @rdname initialize-StockPanel
setMethod(f = "setStockGraphs", signature = c("StockPanel"),
          definition = function(.Object, stockGraphs)
          {
            rightClassElements <- prod(sapply(stockGraphs, function(element) {is(element, "AmGraph")}))
            if(!rightClassElements){
              stop("[setStockGraphs]: each element must be created with stockGraph(*)")
            }else{}
            .Object@stockGraphs <- lapply(stockGraphs, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @param stockGraph \linkS4class{AmGraph}, created with stockGraph(...)
#' @examples
#' addStockGraph(.Object = stockPanel(), comparable = FALSE)
#' addStockGraph(.Object = stockPanel(), stockGraph = stockGraph(TEST = FALSE))
#' @rdname initialize-StockPanel
#' @export
setGeneric(name = "addStockGraph",
           def = function(.Object, stockGraph = NULL, ...){ standardGeneric("addStockGraph") })
#' @rdname initialize-StockPanel
setMethod(f = "addStockGraph", signature = c("StockPanel"),
          definition = function(.Object, stockGraph = NULL, ...)
          {
            if (is.null(stockGraph) && !missing(...)) {
              stockGraph <- stockGraph(...)
            } else {}
            .Object@stockGraphs <- rlist::list.append(.Object@stockGraphs,
                                                      listProperties(stockGraph))
            validObject(.Object)
            return(.Object)
          })

# > @stockLegend : setters ####

#' @examples
#' setStockLegend(.Object = stockPanel(), stockLegend = stockLegend())
#' @rdname initialize-StockPanel
#' @export
setGeneric(name = "setStockLegend",
           def = function(.Object, stockLegend = NULL, ...){ standardGeneric("setStockLegend") })
#' @rdname initialize-StockPanel
setMethod(f = "setStockLegend", signature = c("StockPanel"),
          definition = function(.Object, stockLegend = NULL, ...)
          {
            if (is.null(stockLegend) && !missing(...)) {
              stockLegend <- stockLegend(...)
            } else {}
            .Object@stockLegend<- listProperties(stockLegend)
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' stockPanel(drawnOnAxis = "axis1") 
#' @rdname listProperties-AmObject
setMethod(f = "listProperties", signature = "StockPanel",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length(.Object@drawOnAxis)) {
               ls <- rlist::list.append(ls, drawOnAxis = .Object@drawOnAxis)
             } else {}
             if (length(.Object@stockGraphs)) {
               ls <- rlist::list.append(ls, stockGraphs = .Object@stockGraphs)
             } else {}
             if (length(.Object@stockLegend)) {
               ls <- rlist::list.append(ls, stockLegend = .Object@stockLegend)
             } else {}
             return(ls)
           })