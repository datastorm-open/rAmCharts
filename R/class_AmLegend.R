#' @include class_AmObject.R
NULL

#' @title AmLegend class
#' @author datastorm-open
#' 
#' @description Creates the legend for the chart, automatically adapts the color settings of the graphs.
#' @details Run \code{api("AmLegend")} for more information and all avalaible properties.
#' 
#' @slot useGraphSettings \code{logical}.
#' If TRUE, border color will be changed when user rolls-over the slice, graph, 
#' etc, instead of background color.
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must be a character string.
#' @slot otherProperties \code{list} 
#' containing other avalaible properties not yet implemented in the package.
#' @slot value \code{numeric}.
#' @export
setClass(Class = "AmLegend", contains = "AmObject",
          representation = representation(useGraphSettings = "logical"))

#' @title Initializes legend of the chart
#' @param .Object \linkS4class{AmLegend}.
#' @param useGraphSettings \code{logical}, 
#' if TRUE, border color will be changed when user rolls-over the slice, graph, 
#' etc, instead of background color.
#' @param ... Other properties of \linkS4class{AmLegend}.
#' See \url{http://docs.amcharts.com/3/javascriptstockchart/AmLegend}.
#' 
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
#' @rdname initialize-AmLegend
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


#' @examples 
#' legend(useGraphSettings = FALSE)
#' @rdname initialize-AmLegend
#' @export
legend <- function(useGraphSettings, ...) {
  .Object <- new("AmLegend")
  if (!missing(useGraphSettings)) {
    .Object@useGraphSettings <- useGraphSettings
  } else {}
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return(.Object)
}

#' @title Constructor for StockLegend.
#' @description This method is used for \linkS4class{AmStockChart}.
#' 
#' @param useGraphSettings \code{logical}
#' Legend markers can mirror graph's settings,
#' displaying a line and a real bullet as in the graph itself.
#' Set this property to TRUE if you want to enable this feature.
#' @param valueTextComparing \code{character}
#' @param ...
#' Properties of AmLegend.
#' See \url{http://docs.amcharts.com/3/javascriptstockchart/StockLegend}
#' 
#' @return An \linkS4class{AmLegend} object
#' 
#' @examples
#' stockLegend(useGraphSettings = TRUE)
#' 
#' @export
stockLegend <- function(useGraphSettings, valueTextComparing = "[[percents.value]]%", ...) {
  amLegend(useGraphSettings = useGraphSettings, valueTextComparing = valueTextComparing, ...)
}

# > @useGraphSettings : setters ####

#' @rdname initialize-AmLegend
#' @export
setGeneric(name = "setUseGraphSettings", def = function(.Object, useGraphSettings) { standardGeneric("setUseGraphSettings") })
#' @rdname initialize-AmLegend
#' @examples
#' setUseGraphSettings(.Object = amLegend(), useGraphSettings = TRUE)
setMethod(f = "setUseGraphSettings", signature = c("AmLegend", "logical"),
          definition = function(.Object, useGraphSettings)
          {
            .Object@useGraphSettings <- useGraphSettings
            validObject(.Object)
            return(.Object)
          })

#' @rdname listProperties-AmObject
#' @examples
#' # --- signature 'AmLegend'
#' listProperties(amLegend(useGraphSettings = TRUE))
#' 
setMethod(f = "listProperties", signature = "AmLegend",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length(.Object@useGraphSettings)) {
               ls <- rlist::list.append(ls, useGraphSettings = .Object@useGraphSettings)
             } else {}
             return(ls)
           })