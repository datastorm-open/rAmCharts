#' @include AmObject.R ValueAxis.R
NULL

#' @title ChartCursor class
#' @author DataKnowledge
#' @slot oneBalloonOnly
#' Object of class \code{logical}.
#' If this is set to TRUE, border color instead of background color will be changed when
#' user rolls-over the slice, graph, etc.
#' 
#' @slot valueLineAxis
#' Object of class \code{list}.
#' Properties of Axis of value line. If you set valueLineBalloonEnabled to true,
#' but you have more than one axis, you can use this property
#' to indicate which axis should display balloon.
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
setClass(Class = "ChartCursor", contains = "AmObject",
         representation = representation(
           oneBalloonOnly = "logical", valueLineAxis = "list"
         )
)

#' @title Initialize a ChartCursor
#' @param animationDuration
#' Duration of animation of a line, in seconds.
#' @param oneBalloonOnly
#' Object of class \code{logical}.
#' If this is set to TRUE, border color instead of background color will be changed when
#' user rolls-over the slice, graph, etc.
#' @param valueLineAxis
#' Object of class \code{list}.
#' Properties of Axis of value line. If you set valueLineBalloonEnabled to true,
#' but you have more than one axis, you can use this property
#' to indicate which axis should display balloon.
#' @param ...
#' Properties of ChartCursor.
#' See \url{http://docs.amcharts.com/3/javascriptcharts/ChartCursor}
#' @examples
#' new("ChartCursor", oneBalloonOnly = TRUE)
#' @export
setMethod(f = "initialize", signature = "ChartCursor",
          definition = function(.Object, animationDuration = .3, oneBalloonOnly, valueLineAxis,...)
          {  
            if (!missing(oneBalloonOnly)) {
              .Object@oneBalloonOnly <- oneBalloonOnly
            } else {}
            if (!missing(valueLineAxis)) {
              .Object <- setValueLineAxis(.Object, valueLineAxis)
            } else {}
            .Object <- setProperties(.Object, animationDuration = animationDuration, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####
#' @title Constructor for an ChartCursor
#' @param animationDuration
#' Duration of animation of a line, in seconds.
#' @param oneBalloonOnly
#' Object of class \code{logical}.
#' If this is set to TRUE, border color instead of background color will be changed when
#' user rolls-over the slice, graph, etc.
#' @param valueLineAxis
#' Object of class \code{list}.
#' Properties of Axis of value line. If you set valueLineBalloonEnabled to true,
#' but you have more than one axis, you can use this property
#' to indicate which axis should display balloon.
#' @param ...
#' Properties of ChartCursor.
#' See \url{http://docs.amcharts.com/3/javascriptcharts/ChartCursor}
#' @return An \code{\linkS4class{ChartCursor}} object
#' @examples
#' chartCursor()
#' chartCursor(oneBalloonOnly = TRUE)
#' @export
chartCursor <- function(animationDuration = .3, oneBalloonOnly, valueLineAxis,...) {
  .Object <- new("ChartCursor", animationDuration = animationDuration)
  if (!missing(oneBalloonOnly)) {
    .Object@oneBalloonOnly <- oneBalloonOnly
  } else {}
  if (!missing(valueLineAxis)) {
    .Object <- setValueLineAxis(.Object, valueLineAxis)
  } else {}
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return( .Object )
}

# > @oneBalloonOnly : setters ####

#' @exportMethod setOneBalloonOnly
setGeneric(name = "setOneBalloonOnly", def = function(.Object, oneBalloonOnly){ standardGeneric("setOneBalloonOnly") } )
#' @title SETTER
#' @param .Object
#' Object of class \code{\linkS4class{ChartCursor}}
#' @param oneBalloonOnly
#' Object of class \code{logical}.
#' If this is set to TRUE, border color instead of background color will be changed when
#' user rolls-over the slice, graph, etc.
#' @return Updated .Object
#' @examples
#' library(pipeR)
#' chartCursor() %>>% setOneBalloonOnly(TRUE)
#' @export
setMethod(
  f = "setOneBalloonOnly",
  signature = c("ChartCursor", "logical"),
  definition = function(.Object, oneBalloonOnly)
  {
    .Object@oneBalloonOnly <- oneBalloonOnly
    validObject(.Object)
    return(.Object)
  }
)

# > @valueLineAxis : setters ####

#' @exportMethod setValueLineAxis
setGeneric(name = "setValueLineAxis",
           def = function(.Object, valueLineAxis = NULL, ...){ standardGeneric("setValueLineAxis") } )
#' @title SETTER
#' @param .Object
#' \code{\linkS4class{ChartCursor}}.
#' @param valueLineAxis
#' (optional) Object of class \code{\linkS4class{ValueAxis}}.
#' @param ...
#' Properties of valueAxis
#' @return Updated .Object
#' @examples
#' library(pipeR)
#' chartCursor() %>>% setValueLineAxis( title = "Hello !", axisTitleOffset = 12 )
#' @export
setMethod(
  f = "setValueLineAxis",
  signature = c("ChartCursor"),
  definition = function(.Object, valueLineAxis = NULL, ...)
  {
    if (is.null(valueLineAxis) && !missing(...)) {
      .Object@valueLineAxis <- listProperties(valueAxis(...))
    } else {
      .Object@valueLineAxis <- listProperties(valueLineAxis)
    }
    validObject(.Object)
    return(.Object)
  }
)

#' @title List properties
#' @examples
#' new("ChartCursor", oneBalloonOnly = TRUE)
#' @return Properties of the object in a list
#' @importFrom rlist list.append
setMethod( f = "listProperties", signature = "ChartCursor",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length(.Object@oneBalloonOnly)) {
               ls <- rlist::list.append(ls, oneBalloonOnly = .Object@oneBalloonOnly)
             } else {}
             if(length(.Object@valueLineAxis)){
               ls <- rlist::list.append(ls, valueLineAxis = .Object@valueLineAxis)
             } else {}
             return(ls)
           })