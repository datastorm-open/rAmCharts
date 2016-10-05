#' @include class_AmObject.R
NULL

#' @title Title class
#' @author datastorm-open
#' 
#' @description Creates a title on above the chart, multiple can be assigned.
#' @details Run \code{api("Title")} for more informations and all avalaible properties.
#' 
#' @slot text \code{character}, title's text.
#' @slot size \code{numeric}, title's size.
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must be a character string.
#' See examples for details.
#' @slot otherProperties \code{list}
#' containing other avalaible properties not yet implemented in the package.
#' @slot value \code{numeric}.
#' 
#' @export
setClass(Class = "Title", contains = "AmObject",
         representation =
           representation(text = "character", size = "numeric"))

#' @title Initializes A Title
#' @description Uses the constructor to create the object
#' or update an existing one with the setters.
#' 
#' @param .Object \linkS4class{Title}
#' @param text \code{character}, title text.
#' @param size \code{numeric}, title size.
#' @param ... other properties of Title.
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
#' rAmCharts:::title(text = "Main", size = 10)
#' rAmCharts:::title(text = "Main", bold = TRUE)
title <- function(text, size, ...) {
  .Object <- new("Title", ...)
  if (!missing(text)) .Object@text <- text
  if (!missing(size)) .Object@size <- size

  validObject(.Object)
  return(.Object)
}

# CONSTRUCTOR ####
#' @rdname Title
#' @examples
#' amTitle(text = "Main", size = 10)
#' amTitle(text = "Main", bold = TRUE)
#' @export
amTitle <- function(text, size, ...) {
  .Object <- new("Title", ...)
  if (!missing(text)) .Object@text <- text
  if (!missing(size)) .Object@size <- size
  
  validObject(.Object)
  return(.Object)
}

# > @text : setters ####

#' @examples
#' setText(.Object = amTitle(), text = "Bonjour")
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
#' setSize(amTitle(), 16)
#' @rdname Title
setMethod(f = "setSize", signature = c("Title", "numeric"),
          definition = function(.Object, size)
          {
            .Object@size <- size
            validObject(.Object)
            return(.Object)
          })
