#' @include class_AmObject.R class_GaugeAxis.R
NULL

#' @title GaugeBand class
#' @author datastorm-open
#' 
#' @description Creates a band for a specified value range on the GaugeAxis.
#' Multiple bands can be assigned to a single GaugeAxis.
#' @details Run \code{api("GaugeBand")} for more information and all avalaible properties.
#' 
#' @slot id \code{character}. Unique id of a band.
#' @slot listeners \code{list} containining the listeners to add to the chart.
#' The list must be named as in the official API. Each element must be a character string.
#' @slot otherProperties \code{list}
#' containing other avalaible properties not yet implemented in the package.
#' @slot value \code{numeric}.
#' 
#' @export
setClass(Class = "GaugeBand", contains = "AmObject",
         representation = representation(id = "character")
)

#' @title Initializes a GaugeBand
#' @description Uses the constructor to create the object
#' or update an existing one with the setters.
#' 
#' @param .Object \linkS4class{GaugeBand} (or "GaugeBand" for initialize).
#' @param alpha \code{numeric}.
#' @param id \code{character}.
#' @param ... other properties of GaugeBand.
#' @return (updated) .Object of class \linkS4class{GaugeBand}.
#' 
#' @examples
#' # --- method 'initialize'
#' new("GaugeBand")
#' 
#' @export
#' @rdname GaugeBand
#' 
setMethod(f = "initialize", signature = c("GaugeBand"),
          definition = function(.Object, alpha = 1, id, ...)
          {            
            if( !missing(id) ){
              .Object <- setID (.Object, id)
            }
            .Object <- setProperties(.Object, alpha = alpha, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####

#' @rdname GaugeBand
#' @examples
#' # --- constructor
#' gaugeBand(alpha = 2, id = "band2")
#' 
#' @export
gaugeBand <- function (alpha = 1, id, ...)
{
  .Object <- new(Class="GaugeBand", alpha = alpha)
  if (!missing(id)) {
    .Object <- setID (.Object, id)
  } else {}
  .Object <-  setProperties(.Object, ...)
  return( .Object )
}

#' @rdname GaugeBand
#' @export
setGeneric(name = "setID", def = function(.Object, id) {standardGeneric("setID")})
#' @rdname GaugeBand
#' @examples
#' # --- set the 'id'
#' setID(.Object = gaugeBand(), id = "1")
#' 
setMethod(f = "setID", signature = "GaugeBand",
  definition = function(.Object, id)
  {
    .Object@id <- id
    validObject(.Object)
    return(.Object)
  })

#' @rdname listProperties-AmObject
#' @examples
#' # --- signature 'GaugeBand'
#' lapply(list(gaugeBand(fillAlpha = .4, value = 1), gaugeBand(fillAlpha = .5)), listProperties)
#' 
setMethod(f = "listProperties", signature = "GaugeBand",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length(.Object@id)) {
               ls <- rlist::list.append(ls, id = .Object@id)
             } else {}
             return(ls)
           })
