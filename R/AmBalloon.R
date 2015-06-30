#' @include AmObject.R
NULL

#' @title AmBalloon class
#' @author DataKnowledge
#' @section Slots:
#' @slot \code{adjustBorderColor}: Object of class \code{logical}.
#' If this is set to TRUE, border color instead of background color will be changed when
#' user rolls-over the slice, graph, etc.
#' @slot \code{color}: Object of class \code{character}.
#' Color of text in the balloon.
#' @slot \code{cornerRadius}: Object of class \code{numeric}.
#' Balloon corner radius.
#' @slot \code{cornerRadius}: Object of class \code{character}.
#' Balloon background color. Usually balloon background color is set by the chart.
#' Only if "adjustBorderColor" is "true" this color will be used.
#' @export
#' @family rAmChart classes
setClass( Class = "AmBalloon", contains = "AmObject",
  representation = representation(
    adjustBorderColor = "logical",
    color = "character",
    cornerRadius = "numeric",
    fillColor = "character"
  )
)

#' @title Initialize an AmBalloon
#' @param \code{adjustBorderColor}: Object of class \code{logical}.
#' If this is set to TRUE, border color instead of background color will be changed when
#' user rolls-over the slice, graph, etc.
#' @param \code{color}: Object of class \code{character}.
#' Color of text in the balloon.
#' @param \code{cornerRadius}: Object of class \code{numeric}.
#' Balloon corner radius.
#' @param \code{cornerRadius}: Object of class \code{character}.
#' Balloon background color. Usually balloon background color is set by the chart.
#' Only if "adjustBorderColor" is "true" this color will be used.
#' @param \code{...}: Properties of AmBalloon.
#' See \code{\url{http://docs.amcharts.com/3/javascriptcharts/AmBalloon}}.
#' @return An object of class \code{\linkS4class{AmBalloon}}.
#' @examples
#' new("AmBalloon", cornerRadius = 10)
#' @family AmBalloon methods
#' @seealso \code{\linkS4class{AmBalloon}} S4 class
#' @export
setMethod(f = "initialize", signature = "AmBalloon",
          definition = function(.Object, adjustBorderColor, color, cornerRadius, fillColor,...)
          {  
            if(!missing(adjustBorderColor)){
              .Object@adjustBorderColor <- adjustBorderColor
            }
            if(!missing(color)){
              .Object@color<- color
            }
            if(!missing(cornerRadius)){
              .Object@cornerRadius <- cornerRadius
            }
            if(!missing(fillColor)){
              .Object@fillColor <- fillColor
            }
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          }
)

# CONSTRUCTOR ####
#' @title Constructor for an AmBalloon
#' @param \code{adjustBorderColor}: Object of class \code{logical}.
#' If this is set to TRUE, border color instead of background color will be changed when
#' user rolls-over the slice, graph, etc.
#' @param \code{color}: Object of class \code{character}.
#' Color of text in the balloon.
#' @param \code{cornerRadius}: Object of class \code{numeric}.
#' Balloon corner radius.
#' @param \code{cornerRadius}: Object of class \code{character}.
#' Balloon background color. Usually balloon background color is set by the chart.
#' Only if "adjustBorderColor" is "true" this color will be used.
#' @param \code{...}: Properties of AmBalloon.
#' See \code{\url{http://docs.amcharts.com/3/javascriptcharts/AmBalloon}}.
#' @return The updated \code{\linkS4class{AmBalloon}} object.
#' @examples
#' object <- amBalloon(adjustBorderColor = TRUE)
#' @family AmBalloon methods
#' @family rAmChart class constructors
#' @export
amBalloon <- function(adjustBorderColor, color, cornerRadius, fillColor, ...){
  .Object <- new("AmBalloon")
  if(!missing(adjustBorderColor)){
    .Object@adjustBorderColor <- adjustBorderColor
  }
  if(!missing(color)){
    .Object@color<- color
  }
  if(!missing(cornerRadius)){
    .Object@cornerRadius <- cornerRadius
  }
  if(!missing(fillColor)){
    .Object@fillColor <- fillColor
  }
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return( .Object )
}

# > @adjustBorderColor : setters ####

#' @exportMethod setAdjustBorderColor
setGeneric(name = "setAdjustBorderColor", def = function(.Object, adjustBorderColor){ standardGeneric("setAdjustBorderColor") } )
#' @title Setter for adjustBorderColor
#' @param \code{.Object}:
#' Object of class \code{\linkS4class{AmBalloon}}.
#' @param \code{adjustBorderColor}:
#' Object of class \code{logical}.
#' @return The updated object of class \code{\linkS4class{AmBalloon}}.
#' @examples
#' library(pipeR)
#' amBalloon() %>>% setAdjustBorderColor(TRUE)
#' @family AmBalloon methods
#' @family AmBalloon setters
#' @seealso \code{\linkS4class{AmBalloon}} S4 class
#' @name setAdjustBorderColor
#' @rdname setAdjustBorderColor
#' @export
setMethod(
  f = "setAdjustBorderColor",
  signature = c("AmBalloon", "logical"),
  definition = function(.Object, adjustBorderColor)
  {
    .Object@adjustBorderColor <- adjustBorderColor
    validObject(.Object)
    return(.Object)
  }
)

# > @color : setters ####

#' @exportMethod setColor
setGeneric(name = "setColor", def = function(.Object, color){ standardGeneric("setColor") } )
#' @title Setter for color
#' @param \code{.Object}:
#' Object of class \code{\linkS4class{AmBalloon}}.
#' @param \code{color}:
#' Object of class \code{character}.
#' @examples
#' library(pipeR)
#' amBalloon() %>>% setColor("#000000")
#' @return The updated object of class \code{\linkS4class{AmBalloon}}.
#' @family AmBalloon methods
#' @family AmBalloon Setters
#' @seealso \code{\linkS4class{AmBalloon}} S4 class
#' @name setColor
#' @rdname setColor
#' @export
setMethod(
  f = "setColor",
  signature = c("AmBalloon", "character"),
  definition = function(.Object, color)
  {
    .Object@color <- color
    validObject(.Object)
    return(.Object)
  }
)

# > @cornerRadius : setters ####

#' @exportMethod setCornerRadius
setGeneric(name = "setCornerRadius", def = function(.Object, cornerRadius){ standardGeneric("setCornerRadius") } )
#' @title Setter for corner radius
#' @param \code{.Object}:
#' Object of class \code{\linkS4class{AmBalloon}}.
#' @param \code{cornerRadius}:
#' Object of class \code{numeric}.
#' @return The updated object of class \code{\linkS4class{AmBalloon}}.
#' @examples
#' library(pipeR)
#' amBalloon() %>>% setCornerRadius(5)
#' @family AmBalloon methos
#' @family AmBalloon setters
#' @seealso \code{\linkS4class{AmBalloon}}  S4 class
#' @name setCornerRadius
#' @rdname setCornerRadius
#' @export
setMethod(
  f = "setCornerRadius",
  signature = c("AmBalloon", "numeric"),
  definition = function(.Object, cornerRadius)
  {
    .Object@cornerRadius <- cornerRadius
    validObject(.Object)
    return(.Object)
  }
)

# > @fillColor : setters ####

#' @exportMethod setFillColor
setGeneric(name = "setFillColor", def = function(.Object, fillColor){ standardGeneric("setFillColor") } )
#' @title Setter for fillColor
#' @param \code{.Object}:
#' Object of class \code{\linkS4class{AmBalloon}}.
#' @param \code{fillColor}:
#' Object of class \code{character}.
#' @return The updated object of class \code{\linkS4class{AmBalloon}}.
#' @examples
#' library(pipeR)
#' amBalloon() %>>% setFillColor("#FFFFFF")
#' @family AmBalloon methods
#' @family AmBalloon setters
#' @seealso \code{\linkS4class{AmBalloon}} S4 class
#' @name setFillColor
#' @rdname setFillColor
#' @export
setMethod(
  f = "setFillColor",
  signature = c("AmBalloon", "character"),
  definition = function(.Object, fillColor)
  {
    .Object@fillColor <- fillColor
    validObject(.Object)
    return(.Object)
  }
)

# listProperties ####
#' @title List attributes of an AmGraph object
#' @description This function is used to list attributes before addind to graphs (attribute of AmChart)
#' @examples
#' library(pipeR)
#' amBalloon(adjustBorderColor = TRUE) %>>% listProperties
#' @importFrom rlist list.append
setMethod(f = "listProperties", signature = "AmBalloon",
          definition = function(.Object)
          {
            ls <- callNextMethod()
            if( length(.Object@adjustBorderColor) > 0 ){
              ls <- rlist::list.append(ls, adjustBorderColor = .Object@adjustBorderColor)
            }else{}
            if( length(.Object@color) > 0 ){
              ls <- rlist::list.append(ls, color = .Object@color)
            }else{}
            if( length(.Object@cornerRadius) > 0 ){
              ls <- rlist::list.append(ls, cornerRadius = .Object@cornerRadius)
            }else{}
            if( length(.Object@fillColor) > 0 ){
              ls <- rlist::list.append(ls, fillColor = .Object@fillColor)
            }else{}
            return (ls)
          }
)