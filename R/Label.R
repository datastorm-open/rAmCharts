#' @include AmObject.R sharedGenerics.R
NULL

#' @title Label class
#' @author DataKnowledge
#' 
#' @description Creates a label on the chart which can be placed anywhere, multiple can be assigned.
#' @details Run \code{api("Label")} for more information and all avalaible properties.
#' 
#' @slot bold \code{character}. Specifies if label is bold or not.
#' @slot text \code{character}. Text of a title.
#' @slot x \code{numeric}. X position of a label.
#' @slot y \code{numeric}. Y position of a label.
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' See examples for details.
#' @slot otherProperties \code{list},
#' containing other avalaible properties non coded in the package yet.
#' @slot value \code{numeric}.
#' 
#' @export
setClass(Class = "Label", contains = "AmObject",
         representation = representation(
           text = "character",
           bold = "logical",
           x = "numeric",
           y = "numeric"))

#' @title Initialize
#' @param .Object \linkS4class{Label}.
#' @param bold \code{character}.
#' Specifies if label is bold or not.
#' @param text \code{character}.
#' Text of a title.
#' @param x \code{numeric}.
#' X position of a label.
#' @param y \code{numeric}.
#' Y position of a label.
#' @param ... Other properties.
#' @return (updated) .Object of class \linkS4class{Label}.
#' @rdname initialize-Label
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
#' @describeIn  initialize-Label
#' @examples
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

#' @rdname initialize-Label
#' @export
setGeneric(name = "setBold", def = function(.Object, bold){ standardGeneric("setBold") } )
#' @examples
#' setBold(.Object = label(), bold = TRUE)
#' @rdname initialize-Label
setMethod(f = "setBold", signature = c("Label", "logical"),
          definition = function(.Object, bold)
          {
            .Object@bold <- bold
            validObject(.Object)
            return(.Object)
          })

# > @text : setters ####

#' @examples
#' setText(.Object = label(), text = "Bonjour")
#' @rdname initialize-Label
setMethod(f = "setText", signature = c("Label", "character"),
          definition = function(.Object, text)
          {
            .Object@text <- text
            validObject(.Object)
            return(.Object)
          })

# > @x : setters ####

#' @rdname initialize-Label
#' @export
setGeneric(name = "setX", def = function(.Object, x){ standardGeneric("setX") } )
#' @examples
#' setX(.Object = label(), x = 16)
#' @rdname initialize-Label
setMethod(f = "setX", signature = c("Label", "numeric"),
          definition = function(.Object, x)
          {
            .Object@x <- x
            validObject(.Object)
            return(.Object)
          })

# > @y : setters ####

#' @rdname initialize-Label
#' @export
setGeneric(name = "setY", def = function(.Object, y){ standardGeneric("setY") } )
#' @examples
#' setY(.Object = label(), y = 16)
#' @rdname initialize-Label
setMethod(f = "setY", signature = c("Label", "numeric"),
          definition = function(.Object, y)
          {
            .Object@y <- y
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' listProperties(label(text = "balloonText"))
#' @rdname listProperties-AmObject
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
           })