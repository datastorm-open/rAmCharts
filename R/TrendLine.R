#' @include AmObject.R sharedGenerics.R ValueAxis.R
NULL

#' @title TrendLine class
#' @author DataKnowledge
#' 
#' @slot finalValue
#' Object of class \code{numeric}.
#' Value at which trend line should end.
#' 
#' @slot finalXValue
#' Object of class \code{numeric}.
#' Used by XY chart only. X value at which trend line should end.
#' 
#' @slot initialValue
#' Object of class \code{numeric}.
#' Value from which trend line should start.
#' 
#' @slot initialXValue
#' Object of class \code{numeric}.
#' Used by XY chart only. X value from which trend line should start.
#' 
#' @slot valueAxis
#' Object of class \code{\linkS4class{ValueAxis}}.
#' Value axis of the trend line. Will use first value axis of the chart if not set any.
#' You can use a reference to the value axis object or id of value axis.
#' 
#' @slot valueAxisX
#' Object of class \code{\linkS4class{ValueAxis}}.
#' Used by XY chart only. X axis of trend line.
#' Will use first X axis of the chart if not set any.
#' You can use a reference to the value axis object or id of value axis.
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
setClass( Class = "TrendLine", contains = "AmObject",
          representation =
            representation(
              initialValue = "numeric",
              initialXValue = "numeric",
              finalValue = "numeric",
              finalXValue = "numeric",
              valueAxis = "list",
              valueAxisX = "list"
              )
)

#' @title Initialize a TrendLine
#' @examples
#' new("TrendLine", initialValue = 1, finalValue = 11)
#' 
#' # Other example
#' valueAxis <- valueAxis(title = "Hello !", axisTitleOffset = 12)
#' new("TrendLine", valueAxis = valueAxis)
#' @export
setMethod(f = "initialize", signature = "TrendLine",
          definition = function(.Object,
                                initialValue, initialXValue,
                                finalValue, finalXValue,
                                valueAxis, valueAxisX, ...)
          {  
            if(!missing(initialValue)){
              .Object@initialValue <- initialValue
            }
            if(!missing(initialXValue)){
              .Object@initialXValue <- initialXValue
            }
            if(!missing(finalValue)){
              .Object@finalValue <- finalValue
            }
            if(!missing(finalXValue)){
              .Object@finalXValue <- finalXValue
            }
            if(!missing(valueAxis)){
              .Object <- setValueAxis( .Object, valueAxis )
            }
            if(!missing(valueAxisX)){
              .Object <- setValueAxis( .Object, valueAxisX )
            }
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          }
)

# CONSTRUCTOR ####
#' @title Constructor for an AmGraph
#' @param finalValue
#' Object of class \code{numeric}.
#' Value at which trend line should end.
#' @param finalXValue
#' Object of class \code{numeric}.
#' Used by XY chart only. X value at which trend line should end.
#' @param initialValue
#' Object of class \code{numeric}.
#' Value from which trend line should start.
#' @param initialValue
#' Object of class \code{numeric}.
#' Used by XY chart only. X value from which trend line should start.
#' @param valueAxis
#' Object of class \code{\linkS4class{ValueAxis}}.
#' Value axis of the trend line. Will use first value axis of the chart if not set any.
#' You can use a reference to the value axis object or id of value axis.
#' @param valueAxisX
#' Object of class \code{\linkS4class{ValueAxis}}.
#' Used by XY chart only. X axis of trend line.
#' Will use first X axis of the chart if not set any.
#' You can use a reference to the value axis object or id of value axis.
#' @param ...
#' Properties of TrendLine.
#' See \url{http://docs.amcharts.com/3/javascriptcharts/TrendLine}
#' @return An \code{\linkS4class{TrendLine}} object
#' @examples
#' trendLine(initialValue = 1, finalValue = 11)
#' @export
trendLine <- function(.Object,
                      initialValue, initialXValue,
                      finalValue, finalXValue,
                      valueAxis, valueAxisX, ...){
  .Object <- new("TrendLine")
  if (!missing(initialValue)) {
    .Object@initialValue <- initialValue
  } else {}
  if (!missing(initialXValue)) {
    .Object@initialXValue <- initialXValue
  } else {}
  if (!missing(finalValue)) {
    .Object@finalValue <- finalValue
  } else {}
  if (!missing(finalXValue)) {
    .Object@finalXValue <- finalXValue
  } else {}
  if (!missing(valueAxis)) {
    .Object <- setValueAxis( .Object, valueAxis )
  } else {}
  if (!missing(valueAxisX)){
    .Object <- setValueAxis( .Object, valueAxisX )
  } else {}
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return( .Object )
}

# > @initialValue : setters ####

#' @exportMethod setInitialValue
setGeneric(name = "setInitialValue",
           def = function(.Object, initialValue){ standardGeneric("setInitialValue") } )
#' @title SETTER
#' @examples
#' library(pipeR)
#' trendLine() %>>% setInitialValue(16)
#' @export
setMethod(
  f = "setInitialValue",
  signature = c("TrendLine", "numeric"),
  definition = function(.Object, initialValue)
  {
    .Object@initialValue <- initialValue
    validObject(.Object)
    return(.Object)
  }
)

# > @initialXValue : setters ####

#' @exportMethod setInitialXValue
setGeneric(name = "setInitialXValue",
           def = function(.Object, initialXValue){ standardGeneric("setInitialXValue") } )
#' @title SETTER
#' @examples
#' library(pipeR)
#' trendLine() %>>% setInitialXValue(16)
#' @export
setMethod(
  f = "setInitialXValue",
  signature = c("TrendLine", "numeric"),
  definition = function(.Object, initialXValue)
  {
    .Object@initialXValue <- initialXValue
    validObject(.Object)
    return(.Object)
  }
)

# > @finalValue : setters ####

#' @exportMethod setFinalValue
setGeneric(name = "setFinalValue",
           def = function(.Object, finalValue){ standardGeneric("setFinalValue") } )
#' @title SETTER
#' @examples
#' library(pipeR)
#' trendLine() %>>% setFinalValue(16)
#' @export
setMethod(
  f = "setFinalValue",
  signature = c("TrendLine", "numeric"),
  definition = function(.Object, finalValue)
  {
    .Object@finalValue <- finalValue
    validObject(.Object)
    return(.Object)
  }
)

# > @finalXValue : setters ####

#' @exportMethod setFinalXValue
setGeneric(name = "setFinalXValue",
           def = function(.Object, finalXValue){ standardGeneric("setFinalXValue") } )
#' @title SETTER
#' @examples
#' library(pipeR)
#' trendLine() %>>% setFinalXValue(16)
#' @export
setMethod(
  f = "setFinalXValue",
  signature = c("TrendLine", "numeric"),
  definition = function(.Object, finalXValue)
  {
    .Object@finalXValue <- finalXValue
    validObject(.Object)
    return(.Object)
  }
)

# > @valueAxis : setters ####

#' @title SETTER
#' @examples
#' library(pipeR)
#' setValueAxis(trendLine(), valueAxis(title = "Hello !", axisTitleOffset = 12))
#' setValueAxis(trendLine(), title = "Hello !", axisTitleOffset = 12)
#' @export
setMethod(
  f = "setValueAxis",
  signature = c("TrendLine"),
  definition = function(.Object, valueAxis = NULL, ...)
  {
    if (is.null(valueAxis) && !missing(...)) {
      valueAxis <- valueAxis(...)
    } else {}
    .Object@valueAxis <- listProperties(valueAxis)
    validObject(.Object)
    return(.Object)
  }
)

# > @valueAxisX : setters ####

#' @exportMethod setValueAxisX
setGeneric(name = "setValueAxisX",
           def = function(.Object, valueAxisX = NULL, ...){ standardGeneric("setValueAxisX") } )
#' @title SETTER
#' @examples
#' library(pipeR)
#' trendLine() %>>% setValueAxisX(title = "Hello !", axisTitleOffset = 12)
#' valueAxis <- valueAxis(title = "Hello !", axisTitleOffset = 12)
#' trendLine(valueAxis = valueAxis)
#' @export
setMethod(
  f = "setValueAxisX",
  signature = c("TrendLine"),
  definition = function(.Object, valueAxisX = NULL, ...)
  {
    if( is.null(valueAxisX) && !missing(...) ){
      valueAxisX <- valueAxis(...)
    }else{}
    .Object@valueAxisX <- listProperties(valueAxisX)
    validObject(.Object)
    return(.Object)
  }
)

#' @title List properties
#' @examples
#' trendLine( initialValue = 1, valueAxis = valueAxis(axisTitleOffset = 12, tickLength = 10) )
#' @return Properties of the object in a list
#' @importFrom rlist list.append
setMethod( f = "listProperties", signature = "TrendLine",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length( .Object@initialValue ) > 0 ){
               ls <- rlist::list.append(ls, initialValue = .Object@initialValue)
             }else{}
             if( length( .Object@initialXValue ) > 0 ){
               ls <- rlist::list.append(ls, initialXValue = .Object@initialXValue)
             }else{}
             if( length( .Object@finalValue ) > 0 ){
               ls <- rlist::list.append(ls, finalValue = .Object@finalValue)
             }else{}
             if( length( .Object@initialXValue ) > 0 ){
               ls <- rlist::list.append(ls, finalXValue = .Object@finalXValue)
             }else{}
             if( length( .Object@valueAxis ) > 0 ){
               ls <- rlist::list.append(ls, valueAxis = .Object@valueAxis)
             }else{}
             if( length( .Object@valueAxisX ) > 0 ){
               ls <- rlist::list.append(ls, valueAxisX = .Object@valueAxisX)
             }else{}
             return(ls)
           }
)