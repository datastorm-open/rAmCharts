#' @include AmObject.R StockEvent.R
NULL

#' @title DataSet class
#' @author DataKnowledge
#' @section Slots:
#' 
#' @slot \code{dataProvider}: Object of class \code{list}. The data set data.
#' Important: the data sets need to come pre-ordered in ascending order.
#' Data with incorrect order might result in visual and functional glitches on the chart.
#' 
#' @slot \code{fieldMappings}: Object of class \code{list}.
#' Array of field mappings.
#' Field mapping is an object with fromField and toField properties.
#' fromField is a name of your value field in dataProvider.
#' toField might be chosen freely,
#' it will be used to set value/open/close/high/low fields for the StockGraph.
#' Example: {fromField:"val1", toField:"value"}.
#' 
#' @slot \code{stockEvents}: Object of class \code{list}.
#' Containing properties of stockEvents.
#' 
#' @export
setClass( Class = "DataSet", contains = "AmObject",
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
            if( !missing(dataProvider) ){
              .Object <- setDataProvider( .Object, dataProvider)
            }
            if( !missing(fieldMappings) ){
              .Object <- setFieldMappings( .Object, fieldMappings)
            }
            if( !missing(stockEvents) ){
              .Object <- setStockEvents( .Object, stockEvents)
            }
            .Object <- setProperties(.Object, compared = compared, ...)
            validObject(.Object)
            return(.Object)
          }
)

# CONSTRUCTOR ####
#' @title
#' #’ Constructor.
#' @title Constructor for an DataSet
#' @param \code{...}: Properties of DataSet.
#' See \code{\url{http://docs.amcharts.com/3/javascriptcharts/DataSet}}
#' @return An \code{\linkS4class{DataSet}} object
#' @examples
#' dataSet( categoryField = "categoryField" )
#' @export
dataSet <- function(compared = FALSE, dataProvider, fieldMappings, stockEvents,...){
  .Object <- new( "DataSet", compared = compared )
  if( !missing(dataProvider) ){
    .Object <- setDataProvider( .Object, dataProvider)
  }
  if( !missing(fieldMappings) ){
    .Object <- setFieldMappings( .Object, fieldMappings)
  }
  if( !missing(stockEvents) ){
    .Object <- setStockEvents( .Object, stockEvents )
  }
  .Object <- setProperties( .Object, ... )
  validObject(.Object)
  return( .Object )
}

#' @title Setter for dataProvider
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{dataProvider}: Object of class \code{data.frame}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' # Setter for dataProvider
#' dataSet() %>>% setDataProvider(data.frame(key = c("FR", "US"), value = c(20,10)))
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @name setDataProvider
#' @rdname setDataProvider
#' @export
setMethod( f = "setDataProvider", signature = c("DataSet", "data.frame"),
           definition = function(.Object, dataProvider, keepNA = TRUE)
           {
             .Object@dataProvider <- toList(dataProvider, keepNA)
             validObject(.Object)
             return(.Object)
           }
)
# > @fieldMapping : setters ####
#' @exportMethod addFieldMapping
setGeneric(name = "addFieldMapping", def = function(.Object, ...){ standardGeneric("addFieldMapping") } )
#' @title Add fieldMapping
#' @examples 
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
  }
)

# > @stockEvents : setters ####

#' @exportMethod setStockEvents
setGeneric(name = "setStockEvents", def = function(.Object, stockEvents){ standardGeneric("setStockEvents") } )
#' @title SETTER
#' @export
setMethod(
  f = "setStockEvents",
  signature = c("DataSet", "list"),
  definition = function(.Object, stockEvents)
  {
    rightClassElements <- prod(sapply(stockEvents, function(element) {is(element, "StockEvent")}))
    if ( !rightClassElements ){
      stop("[setStockEvents]: each element of setStockEvents must be of class StockEvent")
    }else{}
    .Object@stockEvents <- lapply( stockEvents, listProperties )
    validObject(.Object)
    return(.Object)
  }
)

#' @exportMethod addStockEvent
setGeneric(name = "addStockEvent", def = function(.Object, stockEvent = NULL, ...){ standardGeneric("addStockEvent") } )
#' @title Add stockEvent 
#' @examples 
#' dataSet()
#' @export
setMethod(
  f = "addStockEvent",
  signature = c("DataSet"),
  definition = function(.Object, stockEvent = NULL, ...)
  {
    if( is.null(stockEvent) ){
      stockEvent <- stockEvent(...)
    }else{}
    .Object@stockEvents <-
      list.append(.Object@stockEvents, listProperties( stockEvent ) )
    validObject(.Object)
    return(.Object)
  }
)

#' @title List properties
#' @return Properties of the object in a list
setMethod( f = "listProperties", signature = "DataSet",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length( .Object@dataProvider ) > 0 ){
               ls <- rlist::list.append(ls, dataProvider = .Object@dataProvider)
             }else{}
             if( length( .Object@fieldMappings ) > 0 ){
               ls <- rlist::list.append(ls, fieldMappings = .Object@fieldMappings)
             }else{}
             if( length( .Object@stockEvents ) > 0 ){
               ls <- rlist::list.append(ls, stockEvents = .Object@stockEvents)
             }else{}
             return(ls)
           }
)