#' @include AmObject.R Guide.R sharedGenerics.R
NULL

#' @title AxisBase class
#' @author DataKnowledge
#' 
#' @slot guides
#' Object of class \code{list}.
#' 
#' @slot listeners
#' Object of class \code{"list"} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string. See examples for details.
#' 
#' @slot otherProperties
#' Object of class \code{"list"},
#' containing other avalaible properties non coded in the package yet.
#' 
#' @slot value
#' Object of class \code{numeric}.
#' Guides belonging to this axis. Use addGuide method
#' 
#' @export
setClass(Class = "AxisBase", contains = "AmObject",
  representation = representation(guides = "list", "VIRTUAL")
)

#' @title Add a Guide
#' @param .Object \linkS4class{AxisBase}
#' @param guide \linkS4class{Guide}
#' @param ... Properties of \code{\linkS4class{Guide}}.
#' @examples
#' library(pipeR)
#' valueAxis(axisTitleOffset = 12, tickLength = 10) %>>%
#' addGuide(fillAlpha = .4, adjustBorderColor = TRUE, gridThickness = 1)
#' @rdname initialize-AxisBase
setMethod(
  f = "addGuide",
  signature = c("AxisBase"),
  definition = function(.Object, guide = NULL, ...)
  {
    if (is.null(guide) && !missing(...)) {
      guide <- guide(...)
    } else {}
    .Object@guides <- rlist::list.append(.Object@guides, listProperties(guide))
    validObject(.Object)
    return(.Object)
  })

#' @rdname listProperties-AmObject
setMethod(f = "listProperties", signature = "AxisBase",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length(.Object@guides)) {
               ls <- rlist::list.append(ls, guides = .Object@guides)
             } else {}
             return(ls)
           })
