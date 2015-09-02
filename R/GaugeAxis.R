#' @include AmObject.R GaugeBand.R
NULL

#' @title GaugeAxis class
#' @author DataKnowledge

#' @slot bands
#' Object of class \code{list}
#' containing properties of one or several \code{\linkS4class{GaugeBand}} objects.
#' Bands are used to draw color fills between specified values.
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
setClass(Class = "GaugeAxis", contains = "AmObject",
         representation = representation( bands = "list" )
)

#' @title Initialize a GaugeAxis
#' @examples
#' new("GaugeAxis", alpha = 1)
#' @export
setMethod(f = "initialize", signature = c("GaugeAxis"),
          definition = function(.Object, axisAlpha = 1, bands, ...)
          {   
            
            # a mieux gerer
            if(missing(bands) ){
              bands <- list( gaugeBand() )
            }else{}
            .Object <- setBands(.Object, bands)
            .Object <- setProperties(.Object, axisAlpha = axisAlpha, ...)
            validObject(.Object)
            return(.Object)
          }
)

# CONSTRUCTOR ####
#' @title Constructor.
#' @title Constructor for a GaugeAxis
#' @param \code{...}: {Properties of GaugeAxis.
#' See \code{\url{http://docs.amcharts.com/3/javascriptcharts/GaugeAxis}}}
#' @return An \code{\linkS4class{GaugeAxis}} object
#' @examples
#' gaugeAxis()
#' @export
gaugeAxis <- function(fillAlpha, axisAlpha = 1, bands, ...){
  .Object <- new(Class="GaugeAxis", axisAlpha = axisAlpha)
  if( !missing(bands) ){
    .Object <- setBands(.Object, bands)
  }
  .Object <-  setProperties(.Object, ...)
  return( .Object )
}


#' @exportMethod setBands
setGeneric(name = "setBands", def = function(.Object, bands){ standardGeneric( "setBands" ) } )
#' @title SETTER
#' @examples
#' bands <- list(gaugeBand(), gaugeBand())
#' gaugeAxis(bands = bands)
#' \dontrun{
#' # error
#' bands <- list(gaugeBand(), test = 1)
#' gaugeAxis(bands = bands)
#' }
#' @export
setMethod(
  f = "setBands",
  signature = c("GaugeAxis", "list"),
  definition = function(.Object, bands)
  {
    rightClassElements <- prod(sapply(bands, function(element) {is(element, "GaugeBand")}))
    if( ! rightClassElements ){
      stop("[setBands]: each elements of bands must be a GaugeBand")
    }else{}
    .Object@bands <- lapply(bands, listProperties)
    validObject(.Object)
    return(.Object)
  }
)

#' @exportMethod addBand
setGeneric(name = "addBand", def = function(.Object, band = NULL, ...){ standardGeneric( "addBand" ) } )
#' @title SETTER
#' @importFrom rlist list.append
#' @export
setMethod(
  f = "addBand",
  signature = c("GaugeAxis"),
  definition = function(.Object, band = NULL, ...)
  {
    if( is.null(band) && !missing(...) ){
      band <- gaugeBand(...)
    }else{}
    .Object@bands <- rlist::list.append( .Object@bands, listProperties(band) )
    return(.Object)
  }
)
#' @title List properties
#' @return Properties of the object in a list
#' @importFrom rlist list.append
setMethod( f = "listProperties", signature = "GaugeAxis",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length(.Object@bands) > 0 ){
               ls <- rlist::list.append(ls, bands = .Object@bands)
             }
             return(ls)
           }
)
