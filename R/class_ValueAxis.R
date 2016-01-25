#' @include class_AxisBase.R utils_sharedGenerics.R
NULL

#' @title ValueAxis class
#' @author DataKnowledge
#' 
#' @description Extension for ValueAxis to create an axis for amSerialChart, amRadarChart,
#' amXYChart charts, multiple can be assigned.
#' Gets automatically populated, one for amSerialChart and two for amXYChart charts,
#' if none has been specified.
#' @details Run \code{api("ValueAxis")} for more information and all avalaible properties.
#' 
#' @slot title \code{character}. Title of the axis.
#' @slot guides \code{list}.
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' See examples for details.
#' @slot otherProperties \code{list},
#' containing other avalaible properties non coded in the package yet.
#' @slot value \code{numeric}.
#' Guides belonging to this axis. Use addGuide method
#' 
#' @export
#' 
setClass(Class = "ValueAxis", contains = "AxisBase",
         representation = representation(title = "character"))

#' @title Initialize
#' @description Creates a ValuesAxis or update its properties.
#' 
#' @param .Object \linkS4class{ValueAxis}.
#' @param title \code{character}.
#' @param guides \code{list} of \linkS4class{Guide}.
#' @param ... Other properties (depend of call function)
#' 
#' @examples
#' 
#' guides <- list(guide(fillAlpha = .4), guide(fillAlpha = .5))
#' new("ValueAxis", title = "Hello !",  gridThickness = 1, guides = guides)
#' 
#' \dontrun{
#' new("ValueAxis", title = "Hello !", 1) # 1 is not take into account
#' 
#' # If one element of guides is not a Guide object, it shows an error
#' guides <- list(guide(fillAlpha = .4), b = 1)
#' new("ValueAxis", title = "Hello !",  gridThickness = 1, guides = guides)
#' }
#' 
#' @rdname initialize-ValueAxis
#' @export
#' 
setMethod(f = "initialize", signature = c("ValueAxis"),
          definition = function(.Object, title, guides, ...)
          {            
            if (!missing(title)) {
              .Object@title <- title
            } else {}
            if (!missing(guides) && is.list(guides)) {
              .Object@guides <- lapply(guides, listProperties)
            } else {}
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####

#' @rdname initialize-ValueAxis
#' 
#' @examples
#' valueAxis(title = "Hello !", axisTitleOffset = 12)
#' 
#' @export
#' 
valueAxis <- function(title, ...) {
  .Object <- new(Class="ValueAxis")
  if (!missing(title)) {
    .Object@title <- title
  } else {}
  .Object <- setProperties(.Object, ...)
  return(.Object)
}

#' @rdname initialize-ValueAxis
#' 
#' @examples
#' setTitle(.Object = valueAxis(), title = "Hello !")
#' 
#' @export
#' 
setMethod(f = "setTitle", signature = c("ValueAxis", "character"),
          definition = function(.Object, title)
          { 
            .Object@title <- title
            validObject(.Object)
            return(.Object)
          })

#' @rdname listProperties-AmObject
#' 
#' @examples
#' library(pipeR)
#' \dontshow{
#' valueAxis(axisTitleOffset = 12, tickLength = 10) %>>% listProperties %>>% class
#' }
#' valueAxis(axisTitleOffset = 12, tickLength = 10, axisTitleOffset = 12) %>>%
#' addGuide(fillAlpha = .4, adjustBorderColor = TRUE, gridThickness = 1) %>>% listProperties
#' 
#' @export
#' 
setMethod(f = "listProperties", signature = "ValueAxis",
          definition = function(.Object)
          { 
            ls <- callNextMethod()
            if (length(.Object@title)) {
              ls <- rlist::list.append(ls, title = .Object@title)
            } else {}
            return(ls)
          })
