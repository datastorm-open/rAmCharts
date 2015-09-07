#' @include AmObject.R GaugeAxis.R
NULL

#' @title GaugeBand class
#' @author DataKnowledge
#' 
#' @slot id
#' Object of class \code{character}
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
#' @param ...
#' Properties of GaugeBand.
#' See \url{http://docs.amcharts.com/3/javascriptcharts/GaugeBand}
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
#' library(pipeR)
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
#' @importFrom rlist list.append
setMethod( f = "listProperties", signature = "GaugeBand",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length(.Object@id) > 0 ){
               ls <- rlist::list.append(ls, id = .Object@id)
             }
             return(ls)
           }
)
