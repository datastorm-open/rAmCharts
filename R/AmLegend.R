#' @include AmObject.R
NULL

#' @title AmLegend class
#' @author DataKnowledge
#' 
#' @description Creates the legend for the chart, automatically adapts the color settings of the graphs.
#' @details Run \code{api("AmLegend")} for more information and all avalaible properties.
#' 
#' @slot useGraphSettings \code{logical}.
#' If this is set to TRUE, border color instead of background color will be changed when
#' user rolls-over the slice, graph, etc.
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' See examples for details.
#' @slot otherProperties \code{list},
#' containing other avalaible properties non coded in the package yet.
#' @slot value \code{numeric}.
#' @export
setClass(Class = "AmLegend", contains = "AmObject",
          representation = representation(useGraphSettings = "logical"))

#' @title Initialize
#' @param .Object \linkS4class{AmLegend}.
#' @param useGraphSettings \code{logical}.
#' If this is set to TRUE, border color instead of background color will be changed when
#' user rolls-over the slice, graph, etc.
#' @param ... Other properties of \linkS4class{AmLegend}.
#' See \url{http://docs.amcharts.com/3/javascriptstockchart/AmLegend}.
#' @examples
#' new("AmLegend", useGraphSettings = TRUE)
#' @rdname initialize-AmLegend
#' @export
setMethod(f = "initialize", signature = "AmLegend",
          definition = function(.Object, useGraphSettings,...)
          {  
            if (!missing(useGraphSettings)) {
              .Object@useGraphSettings <- useGraphSettings
            } else {}
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####

#' @description Constructor for an AmLegend.
#' @examples 
#' amLegend(useGraphSettings = FALSE)
#' @describeIn initialize-AmLegend
#' @export
amLegend <- function(useGraphSettings, ...) {
  .Object <- new("AmLegend")
  if (!missing(useGraphSettings)) {
    .Object@useGraphSettings <- useGraphSettings
  } else {}
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return(.Object)
}

#' @title Constructor for StockLegend.
#' @param useGraphSettings
#' Legend markers can mirror graph's settings,
#' displaying a line and a real bullet as in the graph itself.
#' Set this property to true if you want to enable this feature.
#' @param valueTextComparing \code{character}
#' @param ...
#' Properties of AmLegend.
#' See \url{http://docs.amcharts.com/3/javascriptstockchart/StockLegend}
#' @return An \code{\linkS4class{AmLegend}} object
#' @examples
#' stockLegend(useGraphSettings = TRUE)
#' @export
stockLegend <- function(useGraphSettings, valueTextComparing = "[[percents.value]]%", ...) {
  amLegend(useGraphSettings = useGraphSettings, valueTextComparing = valueTextComparing, ...)
}

# > @useGraphSettings : setters ####

#' @description Setter for useGraphSettings.
#' @examples
#' setUseGraphSettings(.Object = amLegend(), useGraphSettings = TRUE)
#' @rdname initialize-AmLegend
#' @export
setGeneric(name = "setUseGraphSettings", def = function(.Object, useGraphSettings) { standardGeneric("setUseGraphSettings") })
#' @rdname initialize-AmLegend
setMethod(f = "setUseGraphSettings", signature = c("AmLegend", "logical"),
          definition = function(.Object, useGraphSettings)
          {
            .Object@useGraphSettings <- useGraphSettings
            validObject(.Object)
            return(.Object)
          })

#' @description Attributes of an AmLegend object.
#' @examples
#' listProperties(amLegend(useGraphSettings = TRUE))
#' @rdname listProperties-AmObject
setMethod(f = "listProperties", signature = "AmLegend",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length(.Object@useGraphSettings)) {
               ls <- rlist::list.append(ls, useGraphSettings = .Object@useGraphSettings)
             } else {}
             return(ls)
           })