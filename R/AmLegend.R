#' @include AmObject.R
NULL

#' @title AmLegend class
#' @author DataKnowledge
#' 
#' @slot useGraphSettings
#' Object of class \code{logical}.
#' If this is set to TRUE, border color instead of background color will be changed when
#' user rolls-over the slice, graph, etc.
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
#' object of class \code{numeric}.
#' 
#' @export
setClass( Class = "AmLegend", contains = "AmObject",
          representation = representation(useGraphSettings = "logical")
)

#' @title Initialize
#' @examples
#' new("AmLegend", useGraphSettings = TRUE)
#' @export
setMethod(f = "initialize", signature = "AmLegend",
          definition = function(.Object, useGraphSettings, listeners, ...)
          {  
            if (!missing(useGraphSettings)) {
              .Object@useGraphSettings <- useGraphSettings
            } else {}
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          }
)

# CONSTRUCTOR ####
#' @title Constructor for AmLegend.
#' @title Constructor for an AmLegend
#' @param \code{useGraphSettings}: Legend markers can mirror graph’s settings,
#' displaying a line and a real bullet as in the graph itself.
#' Set this property to true if you want to enable this feature.
#' @param \code{...}: {Properties of AmLegend.
#' See \code{\url{http://docs.amcharts.com/3/javascriptcharts/AmLegend}}}
#' @return An \code{\linkS4class{AmLegend}} object
#' @examples
#' amLegend(useGraphSettings = TRUE)
#' @export
amLegend <- function(useGraphSettings, ...){
  .Object <- new("AmLegend")
  if(!missing(useGraphSettings)){
    .Object@useGraphSettings <- useGraphSettings
  }
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return( .Object )
}

#' @title Constructor for StockLegend.
#' @title Constructor for an AmLegend
#' @param \code{useGraphSettings}: Legend markers can mirror graph’s settings,
#' displaying a line and a real bullet as in the graph itself.
#' Set this property to true if you want to enable this feature.
#' @param \code{...}: Properties of AmLegend.
#' See \code{\url{http://docs.amcharts.com/3/javascriptstockchart/StockLegend}}
#' @return An \code{\linkS4class{AmLegend}} object
#' @examples
#' stockLegend( useGraphSettings = TRUE )
#' @export
stockLegend <- function(useGraphSettings, valueTextComparing = "[[percents.value]]%", ...){
  amLegend(useGraphSettings, valueTextComparing = valueTextComparing, ...)
}

# > @useGraphSettings : setters ####

#' @exportMethod setUseGraphSettings
setGeneric(name = "setUseGraphSettings", def = function(.Object, useGraphSettings){ standardGeneric("setUseGraphSettings") } )
#' @title SETTER
#' @examples
#' library(pipeR)
#' amLegend() %>>% setUseGraphSettings(TRUE)
#' @rdname setUseGraphSettings
#' @export
setMethod(
  f = "setUseGraphSettings",
  signature = c("AmLegend", "logical"),
  definition = function(.Object, useGraphSettings)
  {
    .Object@useGraphSettings <- useGraphSettings
    validObject(.Object)
    return(.Object)
  }
)

#' @title List properties
#' @examples
#' is(listProperties(amLegend(useGraphSettings = TRUE)), "list")
#' @return Properties of the object in a list
#' @importFrom rlist list.append
setMethod( f = "listProperties", signature = "AmLegend",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length( .Object@useGraphSettings ) > 0){
               ls <- rlist::list.append(ls, useGraphSettings = .Object@useGraphSettings)
             } else {}
             return(ls)
           }
)