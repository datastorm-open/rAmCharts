#' @include AmObject.R AmGraph.R AmLegend.R ValueAxis.R
NULL

#' @title StockPanel class
#' 
#' @section Slots:
#' 
#' @slot \code{drawOnAxis}: Object of class \code{list}. Containing properties of ValueAxis.
#' Specifies on which value axis user can draw trend lines.
#' Set drawingIconsEnabled to true if you want drawing icons to be visible.
#' First value axis will be used if not set here.
#' You can use a reference to the value axis object or id of value axis.
#' 
#' @slot \code{stockGraphs}: Object of class \code{list}.
#' Each element must be have been created with stockGraph(*)
#' 
#' @slot \code{stockLegend}: Object of class \code{list}.
#' Each element must be have been created with stockLegend(*)
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
#' @param \code{stockGraph}: (optionnal) Object of class \code{\linkS4class{amGraph}}.
#' Use stockGraph(*) constructor
#' @param \code{...}: Properties of the \code{\linkS4class{amGraph}} to add.
#' @examples
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
setMethod( f = "listProperties", signature = "StockPanel",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length( .Object@drawOnAxis ) > 0 ){
               ls <- list.append(ls, drawOnAxis = .Object@drawOnAxis)
             }else{}
             if( length( .Object@stockGraphs ) > 0 ){
               ls <- list.append(ls, stockGraphs = .Object@stockGraphs)
             }else{}
             if( length( .Object@stockLegend ) > 0 ){
               ls <- list.append(ls, stockLegend = .Object@stockLegend)
             }else{}
             return(ls)
           }
)