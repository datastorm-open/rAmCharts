#' @include class_AmObject.R
NULL

#' @title Title class
#' @author DataKnowledge
#' 
#' @description Creates a title on above the chart, multiple can be assigned.
#' @details Run \code{api("Title")} for more information and all avalaible properties.
#' 
#' @slot text \code{character}. Text of a title.
#' @slot size \code{numeric}. Text size of a title.
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must be a character string.
#' See examples for details.
#' @slot otherProperties \code{list}
#' containing other avalaible properties not yet coded in the package.
#' @slot value \code{numeric}.
#' 
#' @export
setClass(Class = "Title", contains = "AmObject",
         representation =
           representation(text = "character", size = "numeric"))

#' @title Initialize A Title
#' @description Use the constructor to create the object
#' or update an existing one with the setters.
#' 
#' @param .Object \linkS4class{Title}
#' @param text \code{character}. Text of a title.
#' @param size \code{numeric}. Text size of a title.
#' @param ... Other properties.
#' 
#' @return (updated) \linkS4class{Title}
#' 
#' @examples
#' new("Title", size = 10)
#' @rdname Title
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
#' @rdname Title
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
#' @rdname Title
setMethod(f = "setText", signature = c("Title", "character"),
          definition = function(.Object, text)
          {
            .Object@text <- text
            validObject(.Object)
            return(.Object)
          })

# > @size : setters ####

#' @rdname Title
#' @export
setGeneric(name = "setSize", def = function(.Object, size) { standardGeneric("setSize") })
#' @examples
#' library(pipeR)
#' title() %>>% setSize(16)
#' @rdname Title
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