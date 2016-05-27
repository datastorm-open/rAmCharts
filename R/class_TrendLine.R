#' @include class_AmObject.R utils_basicClassUnions.R
NULL

#' @title TrendLine class
#' @author datastorm-open
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
#' The list must be named as in the official API. Each element must be a character string.
#' See examples for details.
#' @slot otherProperties \code{list},
#' containing other avalaible properties.
#' @slot value \code{numeric}.
#' 
#' @export
setClass(Class = "TrendLine", contains = "AmObject",
         representation = representation(
           initialValue = "numeric",
           initialXValue = "numeric",
           finalValue = "numeric",
           finalXValue = "numeric",
           valueAxis = "listOrCharacter",
           valueAxisX = "listOrCharacter"
         ))

#' @title Initializes a TrendLine
#' @description Uses the constructor to create the object
#' or update an existing one with the setters.
#' 
#' @param .Object \linkS4class{TrendLine}.
#' @param finalValue \code{numeric}, value at which trend line should end.
#' @param finalXValue \code{numeric}, used by XY chart only. X value at which trend line should end.
#' @param initialValue \code{numeric}, value from which trend line should start.
#' @param initialXValue \code{numeric}, used by XY chart only. X value from which trend line should start.
#' @param valueAxis \linkS4class{ValueAxis}.
#' Value axis of the trend line. Will use first value axis of the chart if not set any.
#' You can use a reference to the value axis object or id of value axis.
#' @param valueAxisX \linkS4class{ValueAxis}.
#' Used by XY chart only. X axis of trend line.
#' Will use first X axis of the chart if not set any.
#' You can use a reference to the value axis object or id of value axis.
#' @param ... other properties of TrendLine.
#' 
#' @return (possibly updated) .Object of class \linkS4class{TrendLine}.
#' 
#' @examples
#' new("TrendLine", initialValue = 1, finalValue = 11)
#' 
#' # Other example
#' valueAxis <- valueAxis(title = "Hello !", axisTitleOffset = 12)
#' new("TrendLine", valueAxis = valueAxis)
#' 
#' @rdname TrendLine
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
              .Object <- setValueAxisX(.Object, valueAxisX)
            }
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####

#' @rdname TrendLine
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
    .Object <- setValueAxisX(.Object, valueAxisX)
  } else {}
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return(.Object)
}
