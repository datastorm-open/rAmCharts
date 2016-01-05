#' @include AmObject.R basicClassUnions.R
NULL

#' @title StockEvent class
#' @author DataKnowledge
#' 
#' @description StockEvent is object which holds information about event (bullet).
#' Values from StockEventsSettings will be used if not set.
#' Stock event bullet's size depends on it's graphs fontSize.
#' When user rolls-over, clicks or rolls-out of the event bullet, AmStockChart dispatches events.
#' @details Run \code{api("StockEvent")} for more information.
#' 
#' @slot stockGraph \linkS4class{AmGraph} containing properties of stockGraph.
#' This is the graph on which event will be displayed.
#' You can use a reference to the stock graph object or id of the graph.
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' See examples for details.
#' @slot otherProperties \code{list},
#' containing other avalaible properties non coded in the package yet.
#' @slot value \code{numeric}.
#' 
#' @export
setClass(Class = "StockEvent", contains = "AmObject",
          representation = representation(stockGraph = "listOrCharacter"))

#' @title Initialize a StockEvent
#' @param .Object \linkS4class{StockEvent}.
#' @param backgroundAlpha \code{numeric}.
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
              .Object <- setStockGraph(.Object, stockGraph)
            } else {}
            .Object <- setProperties(.Object, backgroundAlpha	 = backgroundAlpha, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####
#' @rdname initialize-StockEvent
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