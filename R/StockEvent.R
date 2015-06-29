#' @include AmObject.R AmGraph.R
NULL

#' @title StockEvent class
#' @author DataKnowledge
#' @section Slots:
#' @slot \code{stockGraph}: Object of class \code{list}.
#' Containing properties of stockGraph.
#' This is the graph on which event will be displayed.
#' You can use a reference to the stock graph object or id of the graph.
#' @export
setClass( Class = "StockEvent", contains = "AmObject",
          representation = representation(
            stockGraph = "list"
          )
)

#' @title Initialize a StockEvent
#' @examples
#' new("StockEvent")
#' @export
setMethod(f = "initialize", signature = "StockEvent",
          definition = function(.Object, backgroundAlpha = 1, stockGraph, ...)
          {  
            if(!missing(stockGraph)){
              .Object <- setStockEvents( .Object, stockGraph)
            }
            .Object <- setProperties(.Object, backgroundAlpha	 = backgroundAlpha, ...)
            validObject(.Object)
            return(.Object)
          }
)

# CONSTRUCTOR ####
#' @title
#' #’ Constructor.
#' @title Constructor for an StockEvent
#' @param \code{...}: {Properties of StockEvent.
#' See \code{\url{http://docs.amcharts.com/3/javascriptcharts/StockEvent}}}
#' @return An \code{\linkS4class{StockEvent}} object
#' @examples
#' stockEvent()
#' @export
stockEvent <- function( backgroundAlpha = 1, stockGraph,...){
  .Object <- new( "StockEvent", backgroundAlpha	 = backgroundAlpha	 )
  if( !missing(stockGraph) ){
    .Object <- setStockGraph( .Object, stockGraph )
  }
  .Object <- setProperties( .Object, ... )
  validObject(.Object)
  return( .Object )
}

# > @stockGraph : setters ####

#' @exportMethod setStockGraph
setGeneric(name = "setStockGraph", def = function(.Object, stockGraph = NULL, ...){ standardGeneric("setStockGraph") } )
#' @title SETTER
#' @examples
#' stockEvent() %>>% setStockGraph()
#' @export
setMethod(
  f = "setStockGraph",
  signature = c("StockEvent"),
  definition = function(.Object, stockGraph = NULL, ...)
  {
    if( is.null(stockGraph) ){
      stockGraph <- stockGraph(...)
    }
    .Object@stockGraph <- listProperties(stockGraph)
    validObject(.Object)
    return(.Object)
  }
)

#' @title List properties
#' @return Properties of the object in a list
setMethod( f = "listProperties", signature = "StockEvent",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length( .Object@stockGraph ) > 0 ){
               ls <- list.append(ls, stockGraph = .Object@stockGraph)
             }else{}
             return(ls)
           }
)