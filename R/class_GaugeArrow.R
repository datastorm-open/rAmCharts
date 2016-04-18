#' @include class_AmObject.R utils_basicClassUnions.R
NULL

#' @title GaugeArrow class
#' @author datastorm-open
#' 
#' @description Creates an arrow for AmAngularGaugeChart, multiple can be assigned.
#' @details Run \code{api("GaugeArrow")} for more information and all avalaible properties.
#' 
#' @slot axis \code{list} containing properties of \linkS4class{GaugeAxis}.
#' Axis of the arrow. You can use reference to the axis or id of the axis.
#' If you don't set any axis, the first axis of the chart will be used.
#' @slot listeners \code{list} containining the listeners to add to the chart.
#' The list must be named as in the official API. Each element must be a character string.
#' @slot otherProperties \code{list}
#' containing other avalaible properties not yet implemented in the package.
#' @slot value \code{numeric}.
#' 
#' @export
setClass(Class = "GaugeArrow", contains = "AmObject",
         representation = representation(axis = "listOrCharacter"))

#' @title Initializes a GaugeArrow
#' @description Uses the constructor to create the object with its properties
#' or update an existing one with the setters.
#' 
#' @param .Object \linkS4class{GaugeArrow}.
#' @param alpha \code{numeric}.
#' @param axis \linkS4class{GaugeAxis}.
#' Axis of the arrow. You can use reference to the axis or id of the axis.
#' If you don't set any axis, the first axis of the chart will be used.
#' @param ... other properties of GaugeArrow.
#' 
#' @return (updated) .Object of class \linkS4class{GaugeArrow}.
#' 
#' @examples
#' # --- method initialize
#' new("GaugeArrow", alpha = 2)
#' 
#' @rdname GaugeArrow
#' @export
#' 
setMethod(f = "initialize", signature = c("GaugeArrow"),
          definition = function(.Object, alpha = 1, axis, ...)
          {            
            if (!missing(axis)) {
              .Object <- setAxis(.Object = .Object, axis = axis)
            } else {}
            .Object <- setProperties(.Object, alpha = alpha, ...)
            validObject(.Object)
            return(.Object)
          })

#' @rdname GaugeArrow
#' @examples
#' # --- constructor
#' gaugeArrow(value = 10)
#' 
#' @export
#' 
gaugeArrow <- function(alpha = 1,  axis, ...){
  .Object <- new("GaugeArrow", alpha = alpha)
  if (!missing(axis)) {
    .Object <- setAxis(.Object = .Object, axis = axis)
  } else {}
  .Object <-  setProperties(.Object, ...)
  validObject(.Object)
  return(.Object)
}

#' @rdname listProperties-AmObject
#' @examples
#' lapply(list(gaugeArrow(alpha = .4, value = 1), gaugeArrow(alpha = .5)), listProperties)
setMethod(f = "listProperties", signature = "GaugeArrow",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length(.Object@axis)) {
               ls <- rlist::list.append(ls, axis = .Object@axis)
             } else {}
             return(ls)
           })
