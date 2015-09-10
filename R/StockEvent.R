#' @include AmObject.R AmGraph.R
NULL

#' @title StockEvent class
#' @author DataKnowledge
#' 
#' @slot stockGraph
#' Object of class \code{list}.
#' Containing properties of stockGraph.
#' This is the graph on which event will be displayed.
#' You can use a reference to the stock graph object or id of the graph.
#' 
#' @slot listeners
#' Object of class \code{"list"} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' See examples for details.
#' 
#' @slot otherProperties
#' Object of class \code{"list"},
#' containing other avalaible properties non coded in the package yet.
#' 
#' @slot value
#' Object of class \code{numeric}.
#' 
#' @export
setClass(Class = "StockEvent", contains = "AmObject",
          representation = representation(stockGraph = "list"))

#' @title Initialize a StockEvent
#' @param .Object \linkS4class{StockEvent}.
#' @param backgroundAlpha \code{numeric}
#' @param stockGraph \linkS4class{AmGraph} created with stockGraph(*).
#' This is the graph on which event will be displayed.
#' You can use a reference to the stock graph object or id of the graph.
#' @param ... Other properties.
#' @return (updated) argument .Object of class \linkS4class{StockEvent}.
#' @rdname initialize-StockEvent
#' @examples
#' new("StockEvent")
#' @export
setMethod(f = "initialize", signature = "StockEvent",
          definition = function(.Object, backgroundAlpha = 1, stockGraph, ...)
          {  
            if (!missing(stockGraph)) {
              .Object <- setStockGraph( .Object, stockGraph)
            } else {}
            .Object <- setProperties(.Object, backgroundAlpha	 = backgroundAlpha, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####
#' @describeIn initialize-StockEvent
#' @examples
#' stockEvent()
#' @export
stockEvent <- function(backgroundAlpha = 1, stockGraph,...){
  .Object <- new( "StockEvent", backgroundAlpha	 = backgroundAlpha	 )
  if (!missing(stockGraph)) {
    .Object <- setStockGraph( .Object, stockGraph )
  } else {}
  .Object <- setProperties( .Object, ... )
  validObject(.Object)
  return( .Object )
}

# > @stockGraph : setters ####

#' @rdname initialize-StockEvent
#' @export
setGeneric(name = "setStockGraph", def = function(.Object, stockGraph = NULL, ...) {standardGeneric("setStockGraph")})
#' @examples
#' setStockGraph(.Object = stockEvent(), stockGraph = stockGraph(balloonText = "balloonText"))
#' setStockGraph(.Object = stockEvent(), stockGraph = "stockGraph1")
#' @rdname initialize-StockEvent
setMethod(f = "setStockGraph", signature = c("StockEvent"),
  definition = function(.Object, stockGraph = NULL, ...)
  {
    if (is.null(stockGraph)) {
      stockGraph <- stockGraph(...)
    } else if (is(stockGraph, "AmGraph")) {
      .Object@stockGraph <- listProperties(stockGraph)
    } else if (is.character(stockGraph)) {
      .Object <- setProperties(.Object, stockGraph = stockGraph)
    } else {}
    validObject(.Object)
    return(.Object)
  })

#' @examples
#' listProperties(stockEvent())
#' @rdname listProperties-AmObject
setMethod( f = "listProperties", signature = "StockEvent",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length(.Object@stockGraph)) {
               ls <- rlist::list.append(ls, stockGraph = .Object@stockGraph)
             } else {}
             return(ls)
           })