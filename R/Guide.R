#' @include AmObject.R
NULL

#' @title Guide class
#' @author DataKnowledge
#' 
#' @slot fillAlpha
#' Object of clas \code{numeric}.
#' Specifies if a grid line is placed on the center of a cell or on the beginning of a cell.
#' Possible values are: "start" and "middle" This setting doesn't work if parseDates is set to true.
#' 
#' @slot valueAxis
#' Object of class \code{list},
#' containing properties of a \linkS4class{ValueAxis} class.
#' As you can add guides directly to the chart, you might need to specify 
#' which value axis should be used.
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
setClass(Class = "Guide", contains = "AmObject",
         representation = representation(fillAlpha = "numeric", valueAxis = "list")
)

#' @title Initialize a Guide
#' @examples
#' new("Guide", fillAlpha = 0.1,  gridThickness = 1, value = 1)
#' @export
setMethod(f = "initialize", signature = c("Guide"),
          definition = function(.Object, fillAlpha, valueAxis, value, ...)
          {            
            if (!missing(fillAlpha)) {
              .Object@fillAlpha <- fillAlpha
            }
            if (!missing(valueAxis)) {
              .Object@valueAxis <- listProperties(valueAxis)
            }
            if (!missing(value)) {
              .Object@value <- value
            }
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          }
)

# CONSTRUCTOR ####
#' @title Constructor for a Guide
#' @param ...
#' Properties of Guide.
#' See \url{http://docs.amcharts.com/3/javascriptcharts/Guide}
#' @return An \code{\linkS4class{Guide}} object
#' @examples
#' guide(fillAlpha = .4, value = 1)
#' guide(fillAlpha = .4, adjustBorderColor = TRUE, gridThickness = 1)
#' @export
guide <- function(fillAlpha, valueAxis, value, ...) {
  .Object <- new(Class="Guide")
  if (!missing(fillAlpha)) {
    .Object@fillAlpha <- fillAlpha
  }
  if (!missing(valueAxis)) {
    .Object@valueAxis <- listProperties(valueAxis)
  }
  if (!missing(value)) {
    .Object@value <- value
  }
  .Object <- setProperties(.Object, ...)
  return(.Object)
}

#' @exportMethod setFillAlpha
setGeneric(name = "setFillAlpha", def = function(.Object, fillAlpha) { standardGeneric("setFillAlpha") })
#' @title SETTER
#' @examples
#' library(pipeR)
#' guide() %>>% setFillAlpha(1)
#' @rdname guide
#' @export
setMethod(
  f = "setFillAlpha",
  signature = c("Guide", "numeric"),
  definition = function(.Object, fillAlpha)
  {
    .Object@fillAlpha <- fillAlpha
    validObject(.Object)
    return(.Object)
  }
)

#' @title SETTER
#' @examples
#' library(pipeR)
#' guide() %>>% setValueAxis(list(a = 1))
#' @export
setMethod(
  f = "setValueAxis",
  signature = "Guide",
  definition = function(.Object, valueAxis, ...)
  {
    if (is.list(valueAxis)) {
      .Object@valueAxis <- valueAxis
    } else if (is(valueAxis,"ValueAxis")) {
      .Object@valueAxis <- listProperties(valueAxis)
    } else {}
    validObject(.Object)
    return(.Object)
  }
)

#' @title Add a ValueAxis
#' @param .Object
#' Object of class \code{\linkS4class{Guide}}.
#' @param valueAxis
#' Object of class \code{\linkS4class{ValueAxis}}.
#' @return The updated object of class \code{\linkS4class{Guide}}.
#' @examples
#' library(pipeR)
#' guide() %>>% addValueAxis(axisTitleOffset = 12, tickLength = 10)
#' @family Guide setters
#' @family Guide methods
#' @seealso \code{\linkS4class{Guide}} S4 class
#' @seealso \code{\linkS4class{ValueAxis}} S4 class
#' @name addValueAxis
#' @rdname addValueAxis
#' @importFrom rlist list.append
#' @export
setMethod(f = "addValueAxis", signature = c("Guide"),
          definition = function(.Object, valueAxis = NULL, ...)
          {
            if (is.null(valueAxis) && !missing(...)) {
              valueAxis <- valueAxis(...)
            }
            .Object@valueAxis <- rlist::list.append(.Object@valueAxis, listProperties(valueAxis))
            validObject(.Object)
            return(.Object)
          }
)

#' @title List properties
#' @return Properties of the object in a list
#' @examples
#' lapply(list(guide(fillAlpha = .4, value = 1), guide(fillAlpha = .5)), listProperties)
#' @importFrom rlist list.append
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
          }
)
