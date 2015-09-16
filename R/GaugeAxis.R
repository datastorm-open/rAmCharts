#' @include AmObject.R GaugeBand.R
NULL

#' @title GaugeAxis class
#' @author DataKnowledge
#' @slot bands \code{list}
#' containing properties of one or several \code{\linkS4class{GaugeBand}} objects.
#' Bands are used to draw color fills between specified values.
#' 
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' See examples for details.
#' 
#' @slot otherProperties \code{list},
#' containing other avalaible properties non coded in the package yet.
#' 
#' @slot value \code{numeric}.
#' 
#' @export
setClass(Class = "GaugeAxis", contains = "AmObject",
         representation = representation(bands = "list" ))

#' @title Initialize a GaugeAxis
#' @param .Object \linkS4class{GaugeAxis}.
#' @param axisAlpha \code{numeric}.
#' @param bands \code{list} of \linkS4class{GaugeBand}.
#' Bands are used to draw color fills between specified values.
#' @param ... other properties.
#' @examples
#' new("GaugeAxis", alpha = 1)
#' @rdname initialize-GaugeAxis
#' @export
setMethod(f = "initialize", signature = c("GaugeAxis"),
          definition = function(.Object, axisAlpha = 1, bands, ...)
          {
            # a mieux gerer
            if (missing(bands)) {
              bands <- list(gaugeBand())
            } else{}
            .Object <- setBands(.Object, bands)
            .Object <- setProperties(.Object, axisAlpha = axisAlpha, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####
#' @describeIn initialize-GaugeAxis
#' @examples
#' gaugeAxis()
#' @export
gaugeAxis <- function(axisAlpha = 1, bands, ...) {
  .Object <- new(Class="GaugeAxis", axisAlpha = axisAlpha)
  if (!missing(bands)) {
    .Object <- setBands(.Object, bands)
  } else {}
  .Object <-  setProperties(.Object, ...)
  return(.Object )
}

#' @rdname initialize-GaugeAxis
#' @export
setGeneric(name = "setBands", def = function(.Object, bands){standardGeneric("setBands")})
#' @examples
#' bands <- list(gaugeBand(), gaugeBand())
#' gaugeAxis(bands = bands)
#' \dontrun{
#' # error
#' bands <- list(gaugeBand(), test = 1)
#' gaugeAxis(bands = bands)
#' }
#' @rdname initialize-GaugeAxis
setMethod(f = "setBands", signature = c("GaugeAxis", "list"),
          definition = function(.Object, bands)
          {
            rightClassElements <- prod(sapply(bands, function(element) {is(element, "GaugeBand")}))
            if (! rightClassElements ) {
              stop("[setBands]: each elements of bands must be a GaugeBand")
            } else {}
            .Object@bands <- lapply(bands, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @param band \linkS4class{GaugeBand}.
#' @rdname initialize-GaugeAxis
#' @export
setGeneric(name = "addBand", def = function(.Object, band = NULL, ...){standardGeneric("addBand" ) } )
#' @examples
#' addBand(.Object = gaugeAxis(), band = gaugeBand(test = "foo"))
#' @rdname initialize-GaugeAxis
setMethod(f = "addBand", signature = c("GaugeAxis"),
          definition = function(.Object, band = NULL, ...)
          {
            if (is.null(band) && !missing(...) ) {
              band <- gaugeBand(...)
            } else {}
            .Object@bands <- rlist::list.append(.Object@bands, listProperties(band) )
            return(.Object)
          })

#' @examples
#' listProperties(gaugeAxis())
#' @rdname listProperties-AmObject
setMethod(f = "listProperties", signature = "GaugeAxis",
           definition = function(.Object)
           {
             ls <- callNextMethod()
             if (length(.Object@bands)) {
               ls <- rlist::list.append(ls, bands = .Object@bands)
             } else {}
             return(ls)
           })
