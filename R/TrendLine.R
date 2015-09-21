#' @include AmObject.R sharedGenerics.R ValueAxis.R
NULL

#' @title TrendLine class
#' @author DataKnowledge
#' 
#' @description Creates a trendline for amSerialChart and amXYChart charts which indicates the trend
#' of your data or covers some different purposes. Multiple can be assigned.
#' @details Run \code{api("TrendLine")} for more information and all avalaible properties.
#' 
#' @slot finalValue \code{numeric}. Value at which trend line should end.
#' @slot finalXValue \code{numeric}. Used by XY chart only. X value at which trend line should end.
#' @slot initialValue \code{numeric}. Value from which trend line should start.
#' @slot initialXValue \code{numeric}. Used by XY chart only.
#' X value from which trend line should start.
#' @slot valueAxis \linkS4class{ValueAxis}. Value axis of the trend line.
#' Will use first value axis of the chart if not set any.
#' You can use a reference to the value axis object or id of value axis.
#' @slot valueAxisX \linkS4class{ValueAxis}. Used by XY chart only. X axis of trend line.
#' Will use first X axis of the chart if not set any.
#' You can use a reference to the value axis object or id of value axis.
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' See examples for details.
#' @slot otherProperties \code{list},
#' containing other avalaible properties.
#' @slot value \code{numeric}.
#' 
#' @export
setClass(Class = "TrendLine", contains = "AmObject",
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
#' @param .Object \linkS4class{TrendLine}.
#' @param finalValue \code{numeric}.
#' Value at which trend line should end.
#' @param finalXValue \code{numeric}.
#' Used by XY chart only. X value at which trend line should end.
#' @param initialValue \code{numeric}.
#' Value from which trend line should start.
#' @param initialXValue \code{numeric}.
#' Used by XY chart only. X value from which trend line should start.
#' @param valueAxis \linkS4class{ValueAxis}.
#' Value axis of the trend line. Will use first value axis of the chart if not set any.
#' You can use a reference to the value axis object or id of value axis.
#' @param valueAxisX \linkS4class{ValueAxis}.
#' Used by XY chart only. X axis of trend line.
#' Will use first X axis of the chart if not set any.
#' You can use a reference to the value axis object or id of value axis.
#' @param ... Other properties.
#' @examples
#' new("TrendLine", initialValue = 1, finalValue = 11)
#' 
#' # Other example
#' valueAxis <- valueAxis(title = "Hello !", axisTitleOffset = 12)
#' new("TrendLine", valueAxis = valueAxis)
#' @rdname initialize-TrendLine
#' @export
setMethod(f = "initialize", signature = "TrendLine",
          definition = function(.Object,
                                initialValue, initialXValue,
                                finalValue, finalXValue,
                                valueAxis, valueAxisX, ...)
          {  
            if (!missing(initialValue)) {
              .Object@initialValue <- initialValue
            }
            if (!missing(initialXValue)) {
              .Object@initialXValue <- initialXValue
            }
            if (!missing(finalValue)) {
              .Object@finalValue <- finalValue
            }
            if (!missing(finalXValue)) {
              .Object@finalXValue <- finalXValue
            }
            if (!missing(valueAxis)) {
              .Object <- setValueAxis(.Object, valueAxis)
            }
            if (!missing(valueAxisX)) {
              .Object <- setValueAxis(.Object, valueAxisX)
            }
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####

#' @describeIn initialize-TrendLine
#' @examples
#' trendLine(initialValue = 1, finalValue = 11)
#' @export
trendLine <- function(.Object,
                      initialValue, initialXValue,
                      finalValue, finalXValue,
                      valueAxis, valueAxisX, ...) {
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
    .Object <- setValueAxis(.Object, valueAxis)
  } else {}
  if (!missing(valueAxisX)) {
    .Object <- setValueAxis(.Object, valueAxisX)
  } else {}
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return(.Object)
}

# > @initialValue : setters ####

#' @examples
#' setInitialValue(.Object = trendLine(), initialValue = 16)
#' @rdname initialize-TrendLine
#' @export
setGeneric(name = "setInitialValue",
           def = function(.Object, initialValue) { standardGeneric("setInitialValue") })
#' @rdname initialize-TrendLine
setMethod(f = "setInitialValue", signature = c("TrendLine", "numeric"),
          definition = function(.Object, initialValue)
          {
            .Object@initialValue <- initialValue
            validObject(.Object)
            return(.Object)
          })

# > @initialXValue : setters ####

#' @examples
#' setInitialXValue(.Object = trendLine(), initialXValue = 16)
#' @rdname initialize-TrendLine
#' @export
setGeneric(name = "setInitialXValue",
           def = function(.Object, initialXValue) { standardGeneric("setInitialXValue") })
#' @rdname initialize-TrendLine
setMethod(f = "setInitialXValue", signature = c("TrendLine", "numeric"),
          definition = function(.Object, initialXValue)
          {
            .Object@initialXValue <- initialXValue
            validObject(.Object)
            return(.Object)
          })

# > @finalValue : setters ####

#' @examples
#' setFinalValue(.Object = trendLine(), finalValue = 16)
#' @rdname initialize-TrendLine
#' @export
setGeneric(name = "setFinalValue",
           def = function(.Object, finalValue) { standardGeneric("setFinalValue") })
#' @rdname initialize-TrendLine
setMethod(f = "setFinalValue", signature = c("TrendLine", "numeric"),
          definition = function(.Object, finalValue)
          {
            .Object@finalValue <- finalValue
            validObject(.Object)
            return(.Object)
          })

# > @finalXValue : setters ####

#' @examples
#' setFinalXValue(.Object = trendLine(), finalXValue = 16)
#' @rdname initialize-TrendLine
#' @export
setGeneric(name = "setFinalXValue",
           def = function(.Object, finalXValue) { standardGeneric("setFinalXValue") })
#' @rdname initialize-TrendLine
setMethod(f = "setFinalXValue", signature = c("TrendLine", "numeric"),
          definition = function(.Object, finalXValue)
          {
            .Object@finalXValue <- finalXValue
            validObject(.Object)
            return(.Object)
          })

# > @valueAxis : setters ####

#' @examples
#' setValueAxis(trendLine(), valueAxis(title = "Hello !", axisTitleOffset = 12))
#' setValueAxis(trendLine(), title = "Hello !", axisTitleOffset = 12)
#' @rdname initialize-TrendLine
setMethod(f = "setValueAxis", signature = c("TrendLine"),
          definition = function(.Object, valueAxis = NULL, ...)
          {
            if (is.null(valueAxis) && !missing(...)) {
              valueAxis <- valueAxis(...)
            } else {}
            .Object@valueAxis <- listProperties(valueAxis)
            validObject(.Object)
            return(.Object)
          })

# > @valueAxisX : setters ####

#' @examples
#' setValueAxisX(.Object = trendLine(), title = "Hello !", axisTitleOffset = 12)
#' valueAxis <- valueAxis(title = "Hello !", axisTitleOffset = 12)
#' trendLine(valueAxis = valueAxis)
#' @rdname initialize-TrendLine
#' @export
setGeneric(name = "setValueAxisX",
           def = function(.Object, valueAxisX = NULL, ...) { standardGeneric("setValueAxisX") })
#' @rdname initialize-TrendLine
setMethod(f = "setValueAxisX", signature = c("TrendLine"),
  definition = function(.Object, valueAxisX = NULL, ...)
  {
    if (is.null(valueAxisX) && !missing(...)) {
      valueAxisX <- valueAxis(...)
    } else {}
    .Object@valueAxisX <- listProperties(valueAxisX)
    validObject(.Object)
    return(.Object)
  })

#' @examples
#' trendLine(initialValue = 1, valueAxis = valueAxis(axisTitleOffset = 12, tickLength = 10))
#' @rdname listProperties-AmObject
setMethod(f = "listProperties", signature = "TrendLine",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length(.Object@initialValue)) {
               ls <- rlist::list.append(ls, initialValue = .Object@initialValue)
             } else {}
             if (length(.Object@initialXValue)) {
               ls <- rlist::list.append(ls, initialXValue = .Object@initialXValue)
             } else {}
             if (length(.Object@finalValue)) {
               ls <- rlist::list.append(ls, finalValue = .Object@finalValue)
             } else {}
             if (length(.Object@initialXValue)) {
               ls <- rlist::list.append(ls, finalXValue = .Object@finalXValue)
             } else {}
             if (length(.Object@valueAxis)) {
               ls <- rlist::list.append(ls, valueAxis = .Object@valueAxis)
             } else {}
             if (length(.Object@valueAxisX)) {
               ls <- rlist::list.append(ls, valueAxisX = .Object@valueAxisX)
             } else {}
             return(ls)
           })