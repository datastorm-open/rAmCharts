#' @include AmObject.R StockEvent.R
NULL

#' @title DataSet class
#' @author DataKnowledge
#' 
#' @slot dataProvider
#' Object of class \code{list}. The data set data.
#' Important: the data sets need to come pre-ordered in ascending order.
#' Data with incorrect order might result in visual and functional glitches on the chart.
#' 
#' @slot fieldMappings
#' Object of class \code{list}.
#' Array of field mappings.
#' Field mapping is an object with fromField and toField properties.
#' fromField is a name of your value field in dataProvider.
#' toField might be chosen freely,
#' it will be used to set value/open/close/high/low fields for the StockGraph.
#' Example: {fromField:"val1", toField:"value"}.
#' 
#' @slot stockEvents
#' Object of class \code{list}.
#' Containing properties of stockEvents.
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
setClass(Class = "DataSet", contains = "AmObject",
          representation = representation(
            dataProvider = "list",
            fieldMappings = "list",
            stockEvents = "list"
          )
)

#' @title Initialize a DataSet
#' @examples
#' new("DataSet")
#' @export
setMethod(f = "initialize", signature = "DataSet",
          definition = function(.Object, compared = FALSE, dataProvider, fieldMappings, stockEvents, ...)
          { 
            if (!missing(dataProvider)) {
              .Object <- setDataProvider( .Object, dataProvider)
            } else {}
            if (!missing(fieldMappings)) {
              .Object <- setFieldMappings( .Object, fieldMappings)
            } else {}
            if(!missing(stockEvents)) {
              .Object <- setStockEvents( .Object, stockEvents)
            } else {}
            .Object <- setProperties(.Object, compared = compared, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####
#' @title Constructor for an DataSet
#' @param dataProvider
#' Object of class \code{data.frame}. The data set data.
#' Important: the data sets need to come pre-ordered in ascending order.
#' Data with incorrect order might result in visual and functional glitches on the chart.
#' @param fieldMappings
#' Object of class \code{list}.
#' Array of field mappings.
#' Field mapping is an object with fromField and toField properties.
#' fromField is a name of your value field in dataProvider.
#' toField might be chosen freely,
#' it will be used to set value/open/close/high/low fields for the StockGraph.
#' @param stockEvents
#' Object of class \code{StockEvent}.
#' Containing properties of stockEvents.
#' @param ...
#' Properties of DataSet.
#' See \url{http://docs.amcharts.com/3/javascriptcharts/DataSet}
#' @return An \code{\linkS4class{DataSet}} object
#' @examples
#' dataSet( categoryField = "categoryField" )
#' @export
dataSet <- function(compared = FALSE, dataProvider, fieldMappings, stockEvents,...){
  .Object <- new( "DataSet", compared = compared )
  if (!missing(dataProvider)) {
    .Object <- setDataProvider( .Object, dataProvider)
  } else {}
  if (!missing(fieldMappings)) {
    .Object <- setFieldMappings( .Object, fieldMappings)
  } else {}
  if (!missing(stockEvents)) {
    .Object <- setStockEvents( .Object, stockEvents )
  } else {}
  .Object <- setProperties( .Object, ... )
  validObject(.Object)
  return( .Object )
}

#' @title Setter for dataProvider
#' @param .Object
#' Object of class \code{\linkS4class{DataSet}}.
#' @param dataProvider
#' Object of class \code{data.frame}.
#' @return The updated .Object
#' @examples
#' library(pipeR)
#' dataSet() %>>% setDataProvider(data.frame(key = c("FR", "US"), value = c(20,10)))
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @rdname setDataProvider
#' @export
setMethod( f = "setDataProvider", signature = c("DataSet", "data.frame"),
           definition = function(.Object, dataProvider, keepNA = TRUE)
           {
             .Object@dataProvider <- toList(dataProvider, keepNA)
             validObject(.Object)
             return(.Object)
           })

# > @fieldMapping : setters ####

#' @exportMethod setFieldMappings
setGeneric(name = "setFieldMappings", def = function(.Object, fieldMappings){ standardGeneric("setFieldMappings") } )
#' @title Add fieldMapping
#' @param .Object
#' Object of class \code{\linkS4class{DataSet}}.
#' @param fieldMappings
#' Object of class \code{list}
#' @return The updated \code{.Object}
#' @examples
#' library(pipeR) 
#' dataSet() %>>% addFieldMapping(fromField ="val1", toField ="value")
#' @export
setMethod(
  f = "setFieldMappings",
  signature = c("DataSet", "list"),
  definition = function(.Object, fieldMappings)
  {
    .Object@fieldMappings <- fieldMappings
    validObject(.Object)
    return(.Object)
  })

#' @exportMethod addFieldMapping
setGeneric(name = "addFieldMapping", def = function(.Object, ...){ standardGeneric("addFieldMapping") } )
#' @title Add fieldMapping
#' @param .Object
#' Object of class \code{\linkS4class{DataSet}}.
#' @param ...
#' Properties of fieldMapping.
#' @return The updated .Object
#' @examples
#' library(pipeR) 
#' dataSet() %>>% addFieldMapping(fromField ="val1", toField ="value")
#' @export
setMethod(
  f = "addFieldMapping",
  signature = c("DataSet"),
  definition = function(.Object, ...)
  {
    .Object@fieldMappings <- rlist::list.append(.Object@fieldMappings, list(...))
    validObject(.Object)
    return(.Object)
  })

# > @stockEvents : setters ####

#' @exportMethod setStockEvents
setGeneric(name = "setStockEvents", def = function(.Object, stockEvents){ standardGeneric("setStockEvents") } )
#' @title SETTER
#' @param .Object
#' Object of class \code{\linkS4class{DataSet}}.
#' @param stockEvents
#' Object of class \code{list} containing \code{\linkS4class{StockEvent}}
#' @return The updated .Object
#' @export
setMethod(
  f = "setStockEvents",
  signature = c("DataSet", "list"),
  definition = function(.Object, stockEvents)
  {
    rightClassElements <- prod(sapply(stockEvents, function(element) {is(element, "StockEvent")}))
    if ( !rightClassElements ){
      stop("Each element of setStockEvents must be of class StockEvent")
    }else{}
    .Object@stockEvents <- lapply( stockEvents, listProperties )
    validObject(.Object)
    return(.Object)
  })

#' @exportMethod addStockEvent
setGeneric(name = "addStockEvent", def = function(.Object, stockEvent = NULL, ...){ standardGeneric("addStockEvent") } )
#' @title Add stockEvent
#' @param .Object
#' Object of class \code{\linkS4class{DataSet}}.
#' @param stockEvent
#' (optional) Object of class \code{\linkS4class{StockEvent}}
#' @param ...
#' Properties of \code{\linkS4class{StockEvent}}
#' See \url{http://docs.amcharts.com/3/javascriptstockchart/StockEvent}
#' @examples 
#' dataSet()
#' @importFrom rlist list.append
#' @export
setMethod(
  f = "addStockEvent",
  signature = c("DataSet"),
  definition = function(.Object, stockEvent = NULL, ...)
  {
    if (is.null(stockEvent)) {
      stockEvent <- stockEvent(...)
    } else {}
    .Object@stockEvents <-
      rlist::list.append(.Object@stockEvents, listProperties(stockEvent))
    validObject(.Object)
    return(.Object)
  })

#' @title List properties
#' @param .Object
#' Object of class \code{\linkS4class{DataSet}}.
#' @return Properties of the object in a list
setMethod( f = "listProperties", signature = "DataSet",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length( .Object@dataProvider )) {
               ls <- rlist::list.append(ls, dataProvider = .Object@dataProvider)
             } else {}
             if (length( .Object@fieldMappings )) {
               ls <- rlist::list.append(ls, fieldMappings = .Object@fieldMappings)
             } else {}
             if (length( .Object@stockEvents )) {
               ls <- rlist::list.append(ls, stockEvents = .Object@stockEvents)
             } else {}
             return(ls)
           }
)