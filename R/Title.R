#' @include AmObject.R
NULL

#' @title Title class
#' @author DataKnowledge
#' 
#' @slot text
#' Object of class \code{character}.
#' Text of a title.
#' 
#' @slot size
#' Object of class \code{numeric}.
#' Text size of a title.
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
setClass(Class = "Title", contains = "AmObject",
         representation =
           representation(text = "character", size = "numeric"))

#' @title Initialize A Title
#' @param .Object \code{\linkS4class{Title}}
#' @param text \code{character}.
#' Text of a title.
#' @param size \code{numeric}.
#' Text size of a title.
#' @param ... Other properties.
#' @return (updated) \code{\linkS4class{Title}}
#' @examples
#' new("Title", size = 10)
#' @rdname initialize-Title
#' @export
setMethod(f = "initialize", signature = "Title",
          definition = function(.Object, text, size, ...)
          {  
            if (!missing(text)) {
              .Object@text <- text
            }
            if (!missing(size)) {
              .Object@size <- size
            }
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####
#' @describeIn initialize-Title
#' @examples
#' title(text = "bonjour")
#' @export
title <- function(text, size, ...) {
  .Object <- new("Title")
  if (!missing(text)) {
    .Object@text <- text
  }
  if (!missing(size)) {
    .Object@size <- size
  }
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return(.Object)
}

# > @text : setters ####

#' @examples
#' setText(.Object = title(), text = "Bonjour")
#' @rdname initialize-Title
setMethod(f = "setText", signature = c("Title", "character"),
          definition = function(.Object, text)
          {
            .Object@text <- text
            validObject(.Object)
            return(.Object)
          })

# > @size : setters ####

#' @rdname initialize-Title
#' @export
setGeneric(name = "setSize", def = function(.Object, size) { standardGeneric("setSize") })
#' @examples
#' library(pipeR)
#' title() %>>% setSize(16)
#' @rdname initialize-Title
setMethod(f = "setSize", signature = c("Title", "numeric"),
          definition = function(.Object, size)
          {
            .Object@size <- size
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' title(text = "foo")
#' @rdname listProperties-AmObject
setMethod(f = "listProperties", signature = "Title",
          definition = function(.Object)
          { 
            ls <- callNextMethod()
            if (length(.Object@text)) {
              ls <- rlist::list.append(ls, text = .Object@text)
            } else{}
            if (length(.Object@size)) {
              ls <- rlist::list.append(ls, size = .Object@size)
            } else{}
            return(ls)
          })