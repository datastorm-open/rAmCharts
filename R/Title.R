#' @include AmObject.R
NULL

#' @title Title class
#' @author DataKnowledge
#' @section Slots:
#' @slot \code{text}: {Object of class \code{character}.
#' Text of a title.}
#' @slot \code{size}: {Object of class \code{numeric}.
#' Text size of a title.}
#' @export
setClass( Class = "Title", contains = "AmObject",
          representation =
            representation( text = "character", size = "numeric" )
)

#' @title Initialize A Title
#' @examples
#' new("Title", size = 10)
#' @export
setMethod(f = "initialize", signature = "Title",
          definition = function(.Object, text, size, ...)
          {  
            if(!missing(text)){
              .Object@text <- text
            }
            if(!missing(size)){
              .Object@size <- size
            }
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          }
)

# CONSTRUCTOR ####
#' @title
#' #’ Constructor.
#' @title Constructor for an AmGraph
#' @param \code{...}: {Properties of Title.
#' See \code{\url{http://docs.amcharts.com/3/javascriptcharts/Title}}}
#' @return An \code{\linkS4class{Title}} object
#' @examples
#' new("Title", text = "bonjour")
#' title(text = "bonjour")
#' @export
title <- function(text, size, ...){
  .Object <- new("Title")
  if(!missing(text)){
    .Object@text <- text
  }
  if(!missing(size)){
    .Object@size <- size
  }
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return( .Object )
}

# > @text : setters ####

#' @title SETTER
#' @examples
#' title() %>>% setText("Bonjour")
#' @export
setMethod(
  f = "setText",
  signature = c("Title", "character"),
  definition = function(.Object, text)
  {
    .Object@text <- text
    validObject(.Object)
    return(.Object)
  }
)

# > @size : setters ####

#' @exportMethod setSize
setGeneric(name = "setSize", def = function(.Object, size){ standardGeneric("setSize") } )
#' @title SETTER
#' @examples
#' title() %>>% setSize(16)
#' @export
setMethod(
  f = "setSize",
  signature = c("Title", "numeric"),
  definition = function(.Object, size)
  {
    .Object@size <- size
    validObject(.Object)
    return(.Object)
  }
)

#' @title List properties
#' @examples
#' new("Title", text = "foo")
#' @return Properties of the object in a list
setMethod( f = "listProperties", signature = "Title",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length( .Object@text ) > 0 ){
               ls <- list.append(ls, text = .Object@text)
             }else{}
             if( length( .Object@size ) > 0 ){
               ls <- list.append(ls, size = .Object@size)
             }else{}
             return(ls)
           }
)