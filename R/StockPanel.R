#' @include AmObject.R AmGraph.R AmLegend.R ValueAxis.R
NULL

#' @title StockPanel class
#' 
#' @slot drawOnAxis
#' Object of class \code{list}. Containing properties of ValueAxis.
#' Specifies on which value axis user can draw trend lines.
#' Set drawingIconsEnabled to true if you want drawing icons to be visible.
#' First value axis will be used if not set here.
#' You can use a reference to the value axis object or id of value axis.
#' 
#' @slot stockGraphs
#' Object of class \code{list}.
#' Each element must be have been created with stockGraph(*)
#' 
#' @slot stockLegend
#' Object of class \code{list}.
#' Each element must be have been created with stockLegend(*)
#' 
#' @slot allLabels
#' Object of class \code{"list"}. List of Labels properties.
#' See \code{\linkS4class{Label}}.
#' 
#' @slot arrows
#' Object of class \code{"list"}
#' containing object of class \code{\linkS4class{GaugeArrow}}.
#' 
#' @slot axes
#' Object of class \code{"list"}
#' containing object of class \code{\linkS4class{GaugeAxis}}.
#' 
#' @slot balloon
#' Object of class \code{"list"}.
#' List of an \code{\linkS4class{AmBalloon}} class properties.
#' Creates the balloons (tooltips) of the chart,
#' It follows the mouse cursor when you roll-over the data items.
#' The framework generates the instances automatically you only need to adjust
#' the appearance to your needs.
#' 
#' @slot categoryAxis
#' Object of class \code{"list"}.
#' List of a \code{\linkS4class{CategoryAxis}} properties.
#' Read-only. Chart creates category axis itself.
#' If you want to change some properties,
#' you should get this axis from the chart and set properties to this object.
#' 
#' @slot categoryField
#' Object of class \code{"character"}.
#' Category field name tells the chart the name of the field in your dataProvider object
#' which will be used for category axis values.
#' 
#' @slot ChartCursor
#' Object of class \code{"list"}.
#' List of a \code{\linkS4class{ChartCursor}} class properties.
#' Properties of the chart's cursor.
#' 
#' @slot ChartScrollbar
#' Object of class \code{"list"}.
#' List of a \code{\linkS4class{ChartScrollbar}} class properties.
#' Properties of chart's scrollbar.
#' 
#' @slot creditsPosition
#' Object of class \code{"character"},
#' specifying position of link to amCharts site.
#' Allowed values are: top-left, top-right, bottom-left and bottom-right.
#' 
#' @slot dataProvider
#' Object of class \code{"list"}, containing the data.
#' Use providing method toList* to convert a \code{data.frame}.
#' 
#' @slot graphs
#' Object of class \code{list}.  List of AmGraphs properties
#' See \code{\linkS4class{AmGraph}} class.
#' Creates the visualization of the data in following types: line, column, step line,
#' smoothed line, olhc and candlestick.
#' 
#' @slot graph
#' Object of class \code{\linkS4class{AmGraph}}.
#' Graph of a Gantt chart. Gant chart actually creates multiple graphs (separate for each segment).
#' Properties of this graph are passed to each of the created graphs
#' - this allows you to control the look of segments.
#' 
#' @slot guides
#' Object of class \code{list}.
#' Each elemnt must be of class \code{\linkS4class{Guide}}.
#' Instead of adding guides to the axes, you can push all of them to this array.
#' In case guide has category or date defined, it will automatically will be assigned to the category axis.
#' Otherwise to first value axis, unless you specify a different valueAxes for the guide.
#' 
#' @slot legend
#' Object of class \code{"list"}.
#' List of an \code{\linkS4class{AmLegend}} class properties.
#' Properties of chart's legend.
#' 
#' @slot segmentsField
#' Object of class \code{character}.
#' 
#' @slot subChartProperties
#' Object of class \code{list}
#' 
#' @slot theme
#' Object of class \code{character}.
#' Theme of a chart. Config files of themes can be found in amcharts/themes/ folder.
#' 
#' @slot titles
#' Object of class \code{"list"}. List of Titles properties
#' See \code{\linkS4class{Title}} class.
#' 
#' @slot trendLines
#' Object of class \code{"list"}.
#' List of \code{\linkS4class{TrendLine}} objects added to a chart.
#' You can add trend lines to a chart using this list or access already existing trend lines.
#' 
#' @slot type
#' Object of class \code{"character"}.
#' Possible types are: serial, pie, radar,
#' (types xy, radar, funnel, gauge, map, stock. are in development).
#' 
#' @slot valueAxes Object of class \code{"list"}. List of ValueAxes' properties.
#' See \code{\linkS4class{ValueAxis}} class.
#' Chart creates one value axis automatically,
#' so if you need only one value axis, you don't need to create it.
#' 
#' @slot valueAxis
#' Object of class \code{list}.
#' List of Value axis properties for Gantt chart. Set it's type to "date" if your data is date or time based.
#' In case of Value axis for a Gantt chart. Set it's type to "date" if your data is date or time based.
#' 
#' @slot listeners
#' Object of class \code{"list"} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string. See examples for details.
#' 
#' @slot otherProperties
#' Object of class \code{"list"},
#' containing other avalaible properties non coded in the package yet.
#' 
#' @slot value
#' Object of class \code{numeric}.
#' 
#' @author DataKnowledge
#' @export
setClass( Class = "StockPanel", contains = "AmChart",
          representation = representation(
            drawOnAxis = "list",
            stockGraphs = "list",
            stockLegend = "list"
          )
)

#' @title Initialize a StockPanel
#' @examples
#' new("StockPanel")
#' @export
setMethod(f = "initialize", signature = "StockPanel",
          definition = function(.Object, drawOnAxis, stockGraphs, stockLegend, ...)
          {  
            if(!missing(drawOnAxis)){
              .Object <- setDrawOnAxis( .Object, drawOnAxis)
            }
            if(!missing(stockGraphs)){
              .Object <- setStockGraphs( .Object, stockGraphs)
            }
            if(!missing(stockLegend)){
              .Object <- setStockLegend( .Object, stockLegend)
            }
            .Object <- setProperties( .Object, ... )
            validObject(.Object)
            return(.Object)
          }
)

# CONSTRUCTOR ####
#' @title
#' #’ Constructor.
#' @title Constructor for an StockPanel
#' @param \code{...}: {Properties of StockPanel.
#' See \code{\url{http://docs.amcharts.com/3/javascriptcharts/StockPanel}}}
#' @return An \code{\linkS4class{StockPanel}} object
#' @examples
#' stockPanel()
#' @export
stockPanel <- function( drawOnAxis, stockGraphs, stockLegend, ... ){
  .Object <- new( "StockPanel")
  if(!missing(drawOnAxis)){
    .Object <- setStockDrawOnAxis( .Object, drawOnAxis)
  }
  if(!missing(stockGraphs)){
    .Object <- setStockGraphs( .Object, stockGraphs)
  }
  if(!missing(stockLegend)){
    .Object <- setStockLegend( .Object, stockLegend)
  }
  .Object <- setProperties( .Object, ... )
  validObject(.Object)
  return( .Object )
}

# > @drawOnAxis : setters ####

#' @exportMethod setDrawOnAxis
setGeneric(name = "setDrawOnAxis",
           def = function( .Object, valueAxis = NULL, ... ){ standardGeneric("setDrawOnAxis") } )
#' @title SETTER
#' @examples
#' library(pipeR)
#' stockPanel() %>>% setDrawOnAxis( valueAxis() )
#' @export
setMethod(
  f = "setDrawOnAxis",
  signature = c("StockPanel"),
  definition = function( .Object, valueAxis = NULL, ... )
  {
    if( is.null(valueAxis) && !missing(...)){
      valueAxis <- valueAxis(...)
    }else{}
    .Object@drawOnAxis<- listProperties( valueAxis )
    validObject(.Object)
    return(.Object)
  }
)

# > @stockGraphs : setters ####

#' @exportMethod setStockGraphs
setGeneric(name = "setStockGraphs",
           def = function(.Object, stockGraphs ){ standardGeneric("setStockGraphs") } )
#' @title SETTER
#' @examples
#' library(pipeR)
#' stockPanel() %>>% setStockGraphs( list( stockGraph(comparable = TRUE), stockGraph(comparable = FALSE) ) )
#' @export
setMethod(
  f = "setStockGraphs",
  signature = c("StockPanel"),
  definition = function( .Object, stockGraphs )
  {
    rightClassElements <- prod(sapply(stockGraphs, function(element) {is(element, "AmGraph")}))
    if( !rightClassElements ){
      stop("[setStockGraphs]: each element must be created with stockGraph(*)")
    }else{}
    .Object@stockGraphs <- lapply(stockGraphs, listProperties)
    validObject(.Object)
    return(.Object)
  }
)

#' @exportMethod addStockGraph
setGeneric(name = "addStockGraph",
           def = function(.Object, stockGraph = NULL, ...){ standardGeneric("addStockGraph") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmStockChart}}.
#' @param \code{stockGraph}: (optionnal) Object of class \code{\linkS4class{AmGraph}}.
#' Use stockGraph(*) constructor
#' @param \code{...}: Properties of the \code{\linkS4class{AmGraph}} to add.
#' @examples
#' library(pipeR)
#' stockPanel() %>>% addStockGraph(comparable = FALSE)
#' stockPanel() %>>% addStockGraph( stockGraph(comparable = FALSE) )
#' @return The updated object of class \code{\linkS4class{AmStockChart}}.
#' @family StockPanel setters
#' @family StockPanel methods
#' @seealso \code{\linkS4class{AmStockChart}} S4 class
#' @seealso \code{\linkS4class{AmGraph}} S4 class
#' @name addStockGraph
#' @rdname addStockGraph
#' @export
setMethod( f = "addStockGraph", signature = c("StockPanel"),
           definition = function(.Object, stockGraph = NULL, ...)
           {
             if( is.null(stockGraph) && !missing(...) ){
               stockGraph <- stockGraph(...)
             }else{}
             .Object@stockGraphs <- rlist::list.append(.Object@stockGraphs,
                                                    listProperties(stockGraph))
             validObject(.Object)
             return(.Object)
           }
)

# > @stockLegend : setters ####

#' @exportMethod setStockLegend
setGeneric(name = "setStockLegend",
           def = function( .Object, stockLegend = NULL, ... ){ standardGeneric("setStockLegend") } )
#' @title SETTER
#' @examples
#' library(pipeR)
#' stockPanel() %>>% setStockLegend( stockLegend() )
#' @export
setMethod(
  f = "setStockLegend",
  signature = c("StockPanel"),
  definition = function( .Object, stockLegend = NULL, ... )
  {
    if( is.null(stockLegend) && !missing(...)){
      stockLegend <- stockLegend(...)
    }else{}
    .Object@stockLegend<- listProperties( stockLegend )
    validObject(.Object)
    return(.Object)
  }
)

#' @title List properties
#' @return Properties of the object in a list
#' @importFrom rlist list.append
setMethod( f = "listProperties", signature = "StockPanel",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length( .Object@drawOnAxis ) > 0 ){
               ls <- rlist::list.append(ls, drawOnAxis = .Object@drawOnAxis)
             }else{}
             if( length( .Object@stockGraphs ) > 0 ){
               ls <- rlist::list.append(ls, stockGraphs = .Object@stockGraphs)
             }else{}
             if( length( .Object@stockLegend ) > 0 ){
               ls <- rlist::list.append(ls, stockLegend = .Object@stockLegend)
             }else{}
             return(ls)
           }
)