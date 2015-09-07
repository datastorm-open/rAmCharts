#' @include AmObject.R sharedGenerics.R
NULL

#' @title Label class
#' @author DataKnowledge
#' 
#' @slot bold
#' Object of class \code{character}.
#' Specifies if label is bold or not.
#' 
#' @slot text
#' Object of class \code{character}.
#' Text of a title.
#' 
#' @slot x
#' Object of class \code{numeric}.
#' X position of a label.
#' 
#' @slot y
#' Object of class \code{numeric}.
#' Y position of a label.
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
setClass( Class = "Label", contains = "AmObject",
          representation =
            representation( text = "character", bold = "logical",  x = "numeric", y = "numeric" )
)

#' @title Initialize
#' @examples
#' new("Label", x = 10)
#' @export
setMethod(f = "initialize", signature = "Label",
          definition = function(.Object, text, bold, x, y, ...)
          {  
            if (!missing(text)) {
              .Object@text <- text
            } else {}
            if (!missing(bold)) {
              .Object@bold <- bold
            } else {}
            if (!missing(x)) {
              .Object@x <- x
            } else {}
            if (!missing(y)) {
              .Object@y <- y
            } else {}
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####
#' @title Constructor for an AmGraph
#' @param bold
#' Object of class \code{character}.
#' Specifies if label is bold or not.
#' @param text
#' Object of class \code{character}.
#' Text of a title.
#' @param x
#' Object of class \code{numeric}.
#' X position of a label.
#' @param y
#' Object of class \code{numeric}.
#' Y position of a label.
#' @param ...
#' Properties of Label.
#' See \url{http://docs.amcharts.com/3/javascriptcharts/Label}
#' @return An \code{\linkS4class{Label}} object
#' @examples
#' new("Label", text = "bonjour")
#' label(text = "bonjour")
#' @export
label <- function(text, bold, x, y, ...){
  .Object <- new("Label")
  if (!missing(text)) {
    .Object@text <- text
  } else {}
  if (!missing(bold)) {
    .Object@bold <- bold
  } else {}
  if (!missing(x)) {
    .Object@x <- x
  } else {}
  if (!missing(y)) {
    .Object@y <- y
  } else {}
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return( .Object )
}

# > @bold : setters ####

#' @exportMethod setBold
setGeneric(name = "setBold", def = function(.Object, bold){ standardGeneric("setBold") } )
#' @title SETTER
#' @param .Object
#' Object of class \code{\linkS4class{Label}}
#' @param bold
#' Object of class \code{character}.
#' Specifies if label is bold or not.
#' @return The updated .Object.
#' @examples
#' library(pipeR)
#' label() %>>% setBold(TRUE)
#' @export
setMethod(
  f = "setBold",
  signature = c("Label", "logical"),
  definition = function(.Object, bold)
  {
    .Object@bold <- bold
    validObject(.Object)
    return(.Object)
  })

# > @text : setters ####

#' @title SETTER
#' @param .Object
#' Object of class \code{\linkS4class{Label}}
#' @param text
#' Object of class \code{character}.
#' Text of a title.
#' @return The updated .Object
#' @examples
#' library(pipeR)
#' label() %>>% setText("Bonjour")
#' @export
setMethod(
  f = "setText",
  signature = c("Label", "character"),
  definition = function(.Object, text)
  {
    .Object@text <- text
    validObject(.Object)
    return(.Object)
  })

# > @x : setters ####

#' @exportMethod setX
setGeneric(name = "setX", def = function(.Object, x){ standardGeneric("setX") } )
#' @title SETTER
#' @param .Object
#' Object of class \code{\linkS4class{Label}}
#' @param x
#' Object of class \code{numeric}.
#' X position of a label.
#' @return The updated .Object.
#' @examples
#' library(pipeR)
#' label() %>>% setX(16)
#' @export
setMethod(
  f = "setX",
  signature = c("Label", "numeric"),
  definition = function(.Object, x)
  {
    .Object@x <- x
    validObject(.Object)
    return(.Object)
  })

# > @y : setters ####

#' @exportMethod setY
setGeneric(name = "setY", def = function(.Object, y){ standardGeneric("setY") } )
#' @title SETTER
#' @param .Object
#' Object of class \code{\linkS4class{Label}}
#' @return The updated .Object.
#' @examples
#' library(pipeR)
#' label() %>>% setY(16)
#' @export
setMethod(
  f = "setY",
  signature = c("Label", "numeric"),
  definition = function(.Object, y)
  {
    .Object@y <- y
    validObject(.Object)
    return(.Object)
  })

#' @title List properties
#' @param .Object
#' Object of class \code{\linkS4class{Label}}.
#' @param y
#' Object of class \code{numeric}.
#' Y position of a label.
#' @return The updated .Object.
#' @examples
#' label(text = "balloonText")
#' @return Properties of the object in a list
#' @importFrom rlist list.append
setMethod( f = "listProperties", signature = "Label",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length( .Object@text )) {
               ls <- rlist::list.append(ls, text = .Object@text)
             } else {}
             if (length( .Object@bold )) {
               ls <- rlist::list.append(ls, bold = .Object@bold)
             } else {}
             if (length( .Object@x )) {
               ls <- rlist::list.append(ls, x = .Object@x)
             } else {}
             if (length( .Object@y )) {
               ls <- rlist::list.append(ls, y = .Object@y)
             } else {}
             return(ls)
           }
)