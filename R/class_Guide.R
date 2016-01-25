#' @include class_AmObject.R utils_basicClassUnions.R
NULL

#' @title Guide class
#' @author DataKnowledge
#' 
#' @description Creates a horizontal/vertical guideline-/area for
#' amSerialChart, amXYChart and amRadarChart charts,
#' automatically adapts it's settings from the axes if none has been specified.
#' @details Run \code{api("Guide")} for more information and all avalaible properties.
#' 
#' @slot fillAlpha \code{numeric}.
#' Specifies if a grid line is placed on the center of a cell or on the beginning of a cell.
#' Possible values are: "start" and "middle" This setting doesn't work if parseDates is set to true.
#' @slot valueAxis \linkS4class{ValueAxis}.
#' As you can add guides directly to the chart, you might need to specify 
#' which value axis should be used.
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' See examples for details.
#' @slot otherProperties \code{list},
#' containing other avalaible properties non coded in the package yet.
#' @slot value \code{numeric}.
#' 
#' @export
setClass(Class = "Guide", contains = "AmObject",
         representation = representation(fillAlpha = "numeric", valueAxis = "listOrCharacter"))

#' @title Initialize a Guide
#' @description Use the constructor to create the object
#' or update an existing one with the setters.
#' 
#' @param .Object \linkS4class{Guide}
#' @param fillAlpha \code{numeric}.
#' Specifies if a grid line is placed on the center of a cell or on the beginning of a cell.
#' Possible values are: "start" and "middle"
#' This setting doesn't work if parseDates is set to true.
#' @param valueAxis \linkS4class{ValueAxis} class.
#' As you can add guides directly to the chart, you might need to specify 
#' which value axis should be used.
#' @param value \code{numeric}.
#' @param ... Other properties.
#' 
#' @examples
#' # --- method initialize
#' new("Guide", fillAlpha = 0.1, gridThickness = 1, value = 1)
#' 
#' @export
#' @rdname initialize-Guide
#' 
setMethod(f = "initialize", signature = c("Guide"),
          definition = function(.Object, fillAlpha, valueAxis, value, ...)
          {            
            if (!missing(fillAlpha)) {
              .Object@fillAlpha <- fillAlpha
            } else {}
            if (!missing(valueAxis)) {
              .Object@valueAxis <- listProperties(valueAxis)
            } else {}
            if (!missing(value)) {
              .Object@value <- value
            } else {}
            .Object <- setProperties(.Object, ...)
            
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####

#' @rdname initialize-Guide
#' 
#' @examples
#' # --- constructor
#' guide(fillAlpha = .4, value = 1)
#' guide(fillAlpha = .4, adjustBorderColor = TRUE, gridThickness = 1)
#' 
#' @export
#' 
guide <- function(fillAlpha, valueAxis, value, ...) {
  .Object <- new(Class="Guide")
  if (!missing(fillAlpha)) {
    .Object <- setFillAlpha(.Object = .Object, fillAlpha = fillAlpha)
  } else {}
  if (!missing(valueAxis)) {
    .Object <- setValueAxis(.Object = .Object, valueAxis = valueAxis)
  } else {}
  if (!missing(value)) {
    .Object@value <- value
  } else {}
  .Object <- setProperties(.Object, ...)
  
  validObject(.Object)
  return(.Object)
}

#' @examples
#' # --- signature 'Guide'
#' lapply(list(guide(fillAlpha = .4, value = 1), guide(fillAlpha = .5)), listProperties)
#' @rdname listProperties-AmObject
#' 
setMethod(f = "listProperties", signature = "Guide",
          definition = function(.Object)
          { 
            ls <- callNextMethod()
            if (length(.Object@fillAlpha)) {
              ls <- rlist::list.append(ls, fillAlpha = .Object@fillAlpha)
            } else {}
            if (length(.Object@valueAxis)) {
              ls <- rlist::list.append(ls, valueAxis = .Object@valueAxis)
            } else {}
            if (length(.Object@value)) {
              ls <- rlist::list.append(ls, value = .Object@value)
            }
            return(ls)
          })
