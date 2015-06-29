#' @include AmObject.R sharedGenerics.R
NULL

#' @title Label class
#' @author DataKnowledge
#' @section Slots:
#' @slot \code{text}: {Object of class \code{character}.
#' Text of a title.}
#' @slot \code{x}: {Object of class \code{numeric}.
#' Text x of a title.}
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
            if(!missing(text)){
              .Object@text <- text
            }
            if(!missing(bold)){
              .Object@bold <- bold
            }
            if(!missing(x)){
              .Object@x <- x
            }
            if(!missing(y)){
              .Object@y <- y
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
#' @param \code{...}: {Properties of Label.
#' See \code{\url{http://docs.amcharts.com/3/javascriptcharts/Label}}}
#' @return An \code{\linkS4class{Label}} object
#' @examples
#' new("Label", text = "bonjour")
#' label(text = "bonjour")
#' @export
label <- function(text, bold, x, y, ...){
  .Object <- new("Label")
  if(!missing(text)){
    .Object@text <- text
  }
  if(!missing(bold)){
    .Object@bold <- bold
  }
  if(!missing(x)){
    .Object@x <- x
  }
  if(!missing(y)){
    .Object@y <- y
  }
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return( .Object )
}

# > @bold : setters ####

#' @exportMethod setBold
setGeneric(name = "setBold", def = function(.Object, bold){ standardGeneric("setBold") } )
#' @title SETTER
#' @examples
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
  }
)

# > @text : setters ####

#' @title SETTER
#' @examples
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
  }
)

# > @x : setters ####

#' @exportMethod setX
setGeneric(name = "setX", def = function(.Object, x){ standardGeneric("setX") } )
#' @title SETTER
#' @examples
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
  }
)

# > @x : setters ####

#' @exportMethod setY
setGeneric(name = "setY", def = function(.Object, y){ standardGeneric("setY") } )
#' @title SETTER
#' @examples
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
  }
)

#' @title List properties
#' @examples
#' label(text = "balloonText")
#' @return Properties of the object in a list
setMethod( f = "listProperties", signature = "Label",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length( .Object@text ) > 0 ){
               ls <- list.append(ls, text = .Object@text)
             }
             if( length( .Object@bold ) > 0 ){
               ls <- list.append(ls, bold = .Object@bold)
             }
             if( length( .Object@x ) > 0 ){
               ls <- list.append(ls, x = .Object@x)
             }
             if( length( .Object@y ) > 0 ){
               ls <- list.append(ls, y = .Object@y)
             }
             return(ls)
           }
)