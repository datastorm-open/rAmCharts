#' @include AmObject.R Guide.R sharedGenerics.R
NULL

#' @title AxisBase class
#' @author DataKnowledge
#' @section Slots:
#' @slot \code{guides}: Object of class \code{list}.
#' Guides belonging to this axis. Use addGuide method
#' @export
setClass( Class = "AxisBase", contains = "AmObject",
  representation = representation( guides = "list", "VIRTUAL" )
)

#' @title Add A Guide
#' @param \code{.Object}: Object of class \linkS4class{AxisBase}
#' @param \code{guide}: Object of class \linkS4class{Guide}
#' @examples
#' \dontshow{
#' library(pipeR)
#' valueAxis(axisTitleOffset = 12, tickLength = 10) %>>%
#' addGuide(fillAlpha = .4, adjustBorderColor = TRUE, gridThickness = 1)
#' 
#' valueAxis( axisTitleOffset = 12, tickLength = 10 )
#' }
#' @rdname addGuide
#' @importFrom rlist list.append
#' @export
setMethod(
  f = "addGuide",
  signature = c("AxisBase"),
  definition = function(.Object, guide = NULL, ...)
  {
    if( is.null(guide) && !missing(...) ){
      guide <- guide(...)
    }else{}
    .Object@guides <- rlist::list.append(.Object@guides, listProperties(guide))
    validObject(.Object)
    return(.Object)
  }
)

#' @title List properties
#' @return Properties of the object in a list
#' @importFrom rlist list.append
setMethod( f = "listProperties", signature = "AxisBase",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length( .Object@guides ) > 0 ){
               ls <- rlist::list.append(ls, guides = .Object@guides)
             }
             return(ls)
           }
)
