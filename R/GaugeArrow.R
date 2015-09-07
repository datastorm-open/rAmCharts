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
         representation = representation( axis = "list" )
)

#' @title Initialize a GaugeArrow
#' @examples
#' new("GaugeArrow")
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
          }
)

# CONSTRUCTOR ####
#' @title Constructor for a GaugeArrow
#' @param ...
#' Properties of GaugeArrow.
#' See \url{http://docs.amcharts.com/3/javascriptcharts/GaugeArrow}
#' @return An \code{\linkS4class{GaugeArrow}} object
#' @examples
#' gaugeArrow()
#' @export
gaugeArrow <- function( fillAlpha, alpha = 1,  axis, ... ){
  .Object <- new( "GaugeArrow", alpha = alpha )
  if( !missing(axis) ){
    .Object@axis <- listProperties(axis)
  }
  .Object <-  setProperties(.Object, ...)
  return( .Object )
}


#' @exportMethod setAxis
setGeneric(name = "setAxis", def = function(.Object, axis = NULL, ...){ standardGeneric( "setAxis" ) } )
#' @title SETTER
#' @examples
#' library(pipeR)
#' gaugeArrow() %>>% setAxis()
#' @export
setMethod(
  f = "setAxis",
  signature = "GaugeArrow",
  definition = function(.Object, axis = NULL, ...)
  {
    if( is.null(axis) ){
      axis <- gaugeAxis(...)
    }else{}
      .Object@axis <- listProperties(axis)
    validObject(.Object)
    return(.Object)
  }
)

#' @title List properties
#' @return Properties of the object in a list
#' @examples
#' lapply(list(gaugeArrow(alpha = .4, value = 1), gaugeArrow(alpha = .5)), listProperties)
#' @importFrom rlist list.append
setMethod( f = "listProperties", signature = "GaugeArrow",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length(.Object@axis) > 0 ){
               ls <- rlist::list.append(ls, axis = .Object@axis)
             }
             return(ls)
           }
)
