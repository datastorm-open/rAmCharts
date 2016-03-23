#' @include class_AmObject.R
NULL

#' @title DataSet class
#' @author datastorm-open
#' 
#' @description DataSet is an object which holds all information about data for \linkS4class{AmStockChart}
#' @details Run \code{api("DataSet")} for more information.
#' 
#' @slot dataProvider \code{list}, the data set data.
#' Important: the data sets need to come pre-ordered in ascending order.
#' Data with incorrect order might result in visual and functional glitches on the chart.
#' @slot fieldMappings \code{list}, field mappings.
#' Field mapping is an object with fromField and toField properties.
#' fromField is the name of your value field in dataProvider.
#' toField might be chosen freely,
#' it will be used to set value/open/close/high/low fields for the StockChart.
#' Example: {fromField:"val1", toField:"value"}.
#' @slot stockEvents \code{list} of \linkS4class{StockEvent}.
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must be a character string.
#' @slot otherProperties \code{list}
#' containing other avalaible properties not yet implemented in the package.
#' @slot value \code{numeric}.
#' 
#' @export
#' 
setClass(Class = "DataSet", contains = "AmObject",
          representation = representation(
            dataProvider = "list",
            fieldMappings = "list",
            stockEvents = "list"
          ))


#' @title Creates or updates a DataSet
#' @description Uses the constructors to create the object with its properties
#' or updates an existing one with the setters.
#' 
#' @param .Object \linkS4class{DataSet}.
#' @param compared \code{logical}.
#' @param dataProvider \code{data.frame}, the data set data.
#' Important: the data sets need to come pre-ordered in ascending order.
#' Data with incorrect order might result in visual and functional glitches on the chart.
#' @param fieldMappings \code{list} of field mappings.
#' Field mapping is an object with fromField and toField properties.
#' fromField is a name of your value field in dataProvider.
#' toField might be chosen freely,
#' it will be used to set value/open/close/high/low fields for the StockGraph.
#' Example: list(fromField = "val1", toField ="value").
#' @param stockEvents \linkS4class{StockEvent}.
#' @param ... other properties of DataSet.
#' 
#' @return (updated) \linkS4class{DataSet} object
#' 
#' @examples
#' new("DataSet")
#' 
#' @rdname DataSet
#' @export
#' 
setMethod(f = "initialize", signature = "DataSet",
          definition = function(.Object, compared = FALSE, dataProvider, fieldMappings, stockEvents, ...)
          { 
            if (!missing(dataProvider)) {
              .Object <- setDataProvider( .Object, dataProvider)
            } else {}
            if (!missing(fieldMappings)) {
              .Object <- setFieldMappings( .Object, fieldMappings)
            } else {}
            if (!missing(stockEvents)) {
              .Object <- setStockEvents( .Object, stockEvents)
            } else {}
            .Object <- setProperties(.Object, compared = compared, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####

#' @rdname DataSet
#' 
#' @examples
#' dataSet(categoryField = "categoryField")
#' 
#' @export
#' 
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