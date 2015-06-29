#' @include AmObject.R GaugeAxis.R
NULL

#' @title GaugeBand class
#' @author DataKnowledge
#' @section Slots:
#' @slot \code{id}: Object of class \code{character}
#' Unique id of a band..
#' @export
setClass(Class = "GaugeBand", contains = "AmObject",
         representation = representation( id = "character" )
)

#' @title Initialize a GaugeBand
#' @examples
#' new("GaugeBand")
#' @export
setMethod(f = "initialize", signature = c("GaugeBand"),
          definition = function(.Object, alpha = 1, id, ...)
          {            
            if( !missing(id) ){
              .Object <- setID (.Object, id)
            }
            .Object <- setProperties(.Object, alpha = alpha, ...)
            validObject(.Object)
            return(.Object)
          }
)

# CONSTRUCTOR ####
#' @title Constructor.
#' @title Constructor for a GaugeBand
#' @param \code{...}: {Properties of GaugeBand.
#' See \code{\url{http://docs.amcharts.com/3/javascriptcharts/GaugeBand}}}
#' @return An \code{\linkS4class{GaugeBand}} object
#' @examples
#' gaugeBand()
#' @export
gaugeBand <- function( alpha = 1, id, ... ){
  .Object <- new( Class="GaugeBand", alpha = alpha )
  if( !missing(id) ){
    .Object <- setID (.Object, id)
  }
  .Object <-  setProperties(.Object, ...)
  return( .Object )
}

#' @exportMethod setID
setGeneric(name = "setID", def = function(.Object, id){ standardGeneric( "setID" ) } )
#' @title SETTER
#' @examples
#' gaugeBand() %>>% setID("1")
#' @export
setMethod(
  f = "setID",
  signature = "GaugeBand",
  definition = function(.Object, id)
  {
    .Object@id <- id
    validObject(.Object)
    return(.Object)
  }
)

#' @title List properties
#' @return Properties of the object in a list
#' @examples
#' lapply(list(gaugeBand(fillAlpha = .4, value = 1), gaugeBand(fillAlpha = .5)), listProperties)
setMethod( f = "listProperties", signature = "GaugeBand",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length(.Object@id) > 0 ){
               ls <- list.append(ls, id = .Object@id)
             }
             return(ls)
           }
)
