#' @include class_AmObject.R
NULL

#' @title GaugeAxis class
#' @author DataKnowledge
#' 
#' @description Creates an axis for AmAngularGaugeChart, multiple can be assigned.
#' @details Run \code{api("GaugeAxis")} for more information and all avalaible properties.
#' 
#' @slot bands \code{list} containing properties of one or several \linkS4class{GaugeBand} objects.
#' Bands are used to draw color fills between specified values.
#' @slot listeners \code{list} containining the listeners to add to the chart.
#' The list must be named as in the official API. Each element must be a character string.
#' @slot otherProperties \code{list}
#' containing other avalaible properties not yet implemented in the package.
#' @slot value \code{numeric}.
#' 
#' @export
setClass(Class = "GaugeAxis", contains = "AmObject",
         representation = representation(bands = "list" ))

#' @title Initializes a GaugeAxis
#' @description Uses the constructor to create the object
#' or update an existing one with the setters.
#' 
#' @param .Object \linkS4class{GaugeAxis}.
#' @param axisAlpha \code{numeric}.
#' @param bands \code{list} of \linkS4class{GaugeBand}.
#' Bands are used to draw color fills between specified values.
#' @param ... other properties of GaugeAxis.
#' 
#' @examples
#' # --- method initialize
#' new("GaugeAxis", alpha = 1)
#' 
#' @rdname GaugeAxis
#' @export
#' 
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

#' @rdname GaugeAxis
#' 
#' @examples
#' # -- constructor
#' gaugeAxis()
#' 
#' @export
gaugeAxis <- function(axisAlpha = 1, bands, ...) {
  .Object <- new(Class="GaugeAxis", axisAlpha = axisAlpha)
  if (!missing(bands)) {
    .Object <- setBands(.Object, bands)
  } else {}
  .Object <-  setProperties(.Object, ...)
  return(.Object )
}

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
