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
          ))

#' @title Initialize a DataSet
#' @param .Object \linkS4class{DataSet}.
#' @param compared \code{logical}.
#' @param dataProvider \code{data.frame}. The data set data.
#' Important: the data sets need to come pre-ordered in ascending order.
#' Data with incorrect order might result in visual and functional glitches on the chart.
#' @param fieldMappings \code{list} of field mappings.
#' Field mapping is an object with fromField and toField properties.
#' fromField is a name of your value field in dataProvider.
#' toField might be chosen freely,
#' it will be used to set value/open/close/high/low fields for the StockGraph.
#' Example: list(fromField = "val1", toField ="value").
#' @param stockEvents \linkS4class{StockEvent}.
#' @param ... Other properties.
#' @return (updated) \code{\linkS4class{DataSet}} object
#' @examples
#' new("DataSet")
#' @rdname initialize-DataSet
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

#' @rdname initialize-DataSet
#' @examples
#' dataSet(categoryField = "categoryField")
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

#' @param keepNA \code{logical}.
#' Should the missing values appear in the list ?
#' @examples
#' setDataProvider(.Object = dataSet(), data.frame(key = c("FR", "US"), value = c(20,10)))
#' @rdname initialize-DataSet
setMethod(f = "setDataProvider", signature = c("DataSet", "data.frame"),
          definition = function(.Object, dataProvider, keepNA = TRUE)
          {
            .Object@dataProvider <- toList(dataProvider, keepNA)
            validObject(.Object)
            return(.Object)
          })

# > @fieldMapping : setters ####

#' @rdname initialize-DataSet
#' @export
setGeneric(name = "setFieldMappings", def = function(.Object, fieldMappings) {standardGeneric("setFieldMappings")})
#' @examples
#' addFieldMapping(.Object = dataSet(), fieldMappings = list(fromField ="val1", toField ="value"))
#' @rdname initialize-DataSet
setMethod(f = "setFieldMappings", signature = c("DataSet", "list"),
          definition = function(.Object, fieldMappings)
          {
            .Object@fieldMappings <- fieldMappings
            validObject(.Object)
            return(.Object)
          })

#' @rdname initialize-DataSet
#' @export
setGeneric(name = "addFieldMapping", def = function(.Object, ...) {standardGeneric("addFieldMapping")})
#' @examples
#' addFieldMapping(.Object = dataSet(), fromField ="val1", toField ="value")
#' @rdname initialize-DataSet
setMethod(f = "addFieldMapping", signature = c("DataSet"),
          definition = function(.Object, ...)
          {
            .Object@fieldMappings <- rlist::list.append(.Object@fieldMappings, list(...))
            validObject(.Object)
            return(.Object)
          })

# > @stockEvents : setters ####

#' @rdname initialize-DataSet
#' @export
setGeneric(name = "setStockEvents", def = function(.Object, stockEvents) {standardGeneric("setStockEvents")})
#' @rdname initialize-DataSet
setMethod(f = "setStockEvents", signature = c("DataSet", "list"),
          definition = function(.Object, stockEvents)
          {
            rightClassElements <- prod(sapply(stockEvents, function(element) {is(element, "StockEvent")}))
            if (!rightClassElements) {
              stop("Each element of setStockEvents must be of class StockEvent")
            } else {}
            .Object@stockEvents <- lapply( stockEvents, listProperties )
            validObject(.Object)
            return(.Object)
          })

#' @param stockEvent \linkS4class{StockEvent}.
#' @rdname initialize-DataSet
#' @export
setGeneric(name = "addStockEvent", def = function(.Object, stockEvent = NULL, ...) {standardGeneric("addStockEvent")})
#' @rdname initialize-DataSet
setMethod(f = "addStockEvent", signature = c("DataSet"),
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

#' @rdname listProperties-AmObject
setMethod( f = "listProperties", signature = "DataSet",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length( .Object@dataProvider)) {
               ls <- rlist::list.append(ls, dataProvider = .Object@dataProvider)
             } else {}
             if (length( .Object@fieldMappings)) {
               ls <- rlist::list.append(ls, fieldMappings = .Object@fieldMappings)
             } else {}
             if (length( .Object@stockEvents)) {
               ls <- rlist::list.append(ls, stockEvents = .Object@stockEvents)
             } else {}
             return(ls)
           })