#' @include AmObject.R GaugeAxis.R
NULL

#' @title GaugeArrow class
#' @author DataKnowledge

#' @slot axis
#' Object of class \code{list}
#' containing properties of \code{\linkS4class{GaugeAxis}}.
#' Axis of the arrow. You can use reference to the axis or id of the axis.
#' If you don't set any axis, the first axis of a chart will be used.
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
setClass(Class = "GaugeArrow", contains = "AmObject",
         representation = representation( axis = "list" ))

#' @title Initialize a GaugeArrow
#' @param .Object \linkS4class{GaugeArrow}.
#' @param alpha \code{numeric}.
#' @param axis \linkS4class{GaugeAxis}.
#' Axis of the arrow. You can use reference to the axis or id of the axis.
#' If you don't set any axis, the first axis of a chart will be used.
#' @param ... Other properties.
#' @return (updated) .Object of class \linkS4class{GaugeArrow}.
#' @examples
#' new("GaugeArrow")
#' @rdname initialize-GaugeArrow
#' @export
setMethod(f = "initialize", signature = c("GaugeArrow"),
          definition = function(.Object, alpha = 1, axis, ...)
          {            
            if( !missing(axis) ){
              .Object <- setAxis (.Object, axis = axis)
            }
            .Object <- setProperties(.Object, alpha = alpha, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####

#' @describeIn initialize-GaugeArrow
#' @examples
#' gaugeArrow()
#' @export
gaugeArrow <- function(alpha = 1,  axis, ... ){
  .Object <- new( "GaugeArrow", alpha = alpha )
  if (!missing(axis)) {
    .Object@axis <- listProperties(axis)
  } else {}
  .Object <-  setProperties(.Object, ...)
  return( .Object )
}

#' @rdname initialize-GaugeArrow
#' @export
setGeneric(name = "setAxis", def = function(.Object, axis = NULL, ...) {standardGeneric("setAxis")})
#' @examples
#' setAxis(.Object = gaugeArrow(), axis = gaugeAxis())
#' @rdname initialize-GaugeArrow
setMethod(f = "setAxis", signature = "GaugeArrow",
  definition = function(.Object, axis = NULL, ...)
  {
    if( is.null(axis) ){
      axis <- gaugeAxis(...)
    }else{}
      .Object@axis <- listProperties(axis)
    validObject(.Object)
    return(.Object)
  })

#' @rdname listProperties-AmObject
#' @examples
#' lapply(list(gaugeArrow(alpha = .4, value = 1), gaugeArrow(alpha = .5)), listProperties)
setMethod( f = "listProperties", signature = "GaugeArrow",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length(.Object@axis)){
               ls <- rlist::list.append(ls, axis = .Object@axis)
             } else {}
             return(ls)
           })
