#' @include AmObject.R
NULL

#' @title AmBalloon class
#' @author DataKnowledge
#' 
#' @slot adjustBorderColor : Object of class \code{logical}.
#' If this is set to TRUE, border color instead of background color will be changed when
#' user rolls-over the slice, graph, etc.
#' 
#' @slot color : Object of class \code{character}.
#' Color of text in the balloon.
#' 
#' @slot cornerRadius : Object of class \code{numeric}.
#' Balloon corner radius.
#' 
#' @slot fillColor : Object of class \code{character}.
#' Balloon background color. Usually balloon background color is set by the chart.
#' Only if "adjustBorderColor" is "true" this color will be used.
#' 
#' @slot listeners : Object of class \code{"list"} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' See examples for details.
#' Inherited from \code{\linkS4class{AmObject}}.
#' 
#' @slot otherProperties : Object of class \code{"list"},
#' containing other avalaible properties non coded in the package yet.
#' Inherited from \code{\linkS4class{AmObject}}.
#' 
#' @slot value : object of class \code{numeric}.
#' Inherited from \code{\linkS4class{AmObject}}.
#' 
#' @export
#' @family rAmChart classes
setClass(Class = "AmBalloon", contains = "AmObject",
         representation = representation(
           adjustBorderColor = "logical",
           color = "character",
           cornerRadius = "numeric",
           fillColor = "character"
         )
)

#' @title Initialize an AmBalloon
#' @param adjustBorderColor
#' Object of class \code{logical}.
#' If this is set to TRUE, border color instead of background color will be changed when
#' user rolls-over the slice, graph, etc.
#' @param color
#' Object of class \code{character}.
#' Color of text in the balloon.
#' @param cornerRadius
#' Object of class \code{numeric}.
#' Balloon corner radius.
#' @param fillColor
#' Object of class \code{character}.
#' Balloon background color. Usually balloon background color is set by the chart.
#' Only if "adjustBorderColor" is "true" this color will be used.
#' @param ...
#' Other properties of AmBalloon.
#' See \url{http://docs.amcharts.com/3/javascriptcharts/AmBalloon}.
#' @return An object of class \code{\linkS4class{AmBalloon}}.
#' @examples
#' new("AmBalloon", cornerRadius = 10)
#' @family AmBalloon methods
#' @seealso \code{\linkS4class{AmBalloon}} S4 class
#' @export
setMethod(f = "initialize", signature = "AmBalloon",
          definition = function(.Object, adjustBorderColor, color, cornerRadius, fillColor, ...)
          {  
            if (!missing(adjustBorderColor)) {
              .Object <- setAdjustBorderColor(.Object = .Object, adjustBorderColor = adjustBorderColor)
            } else {}
            if (!missing(color)) {
              .Object <- setColor(.Object = .Object, color = color)
            } else {}
            if (!missing(cornerRadius)) {
              .Object <- setCornerRadius(.Object = .Object, cornerRadius = cornerRadius)
            } else {}
            if (!missing(fillColor)) {
              .Object <- setFillColor(.Object = .Object, fillColor = fillColor)
            } else {}
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####
#' @title Constructor for an AmBalloon
#' 
#' @param adjustBorderColor: Object of class \code{logical}.
#' If this is set to TRUE, border color instead of background color will be changed when
#' user rolls-over the slice, graph, etc.
#' @param color: Object of class \code{character}.
#' Color of text in the balloon.
#' @param cornerRadius: Object of class \code{numeric}.
#' Balloon corner radius.
#' @param fillColor: Object of class \code{character}.
#' Balloon background color. Usually balloon background color is set by the chart.
#' Only if "adjustBorderColor" is "true" this color will be used.
#' @param ...: Properties of AmBalloon.
#' See \url{http://docs.amcharts.com/3/javascriptcharts/AmBalloon}.
#' 
#' @return The updated \code{\linkS4class{AmBalloon}} object.
#' @examples
#' object <- amBalloon(adjustBorderColor = TRUE)
#' @family AmBalloon methods
#' @family rAmChart class constructors
#' @export
amBalloon <- function(adjustBorderColor, color, cornerRadius, fillColor, ...){
  .Object <- new("AmBalloon")
  if (!missing(adjustBorderColor)){
    .Object@adjustBorderColor <- adjustBorderColor
  } else {}
  if (!missing(color)){
    .Object@color<- color
  } else {}
  if (!missing(cornerRadius)){
    .Object@cornerRadius <- cornerRadius
  } else {}
  if (!missing(fillColor)){
    .Object@fillColor <- fillColor
  } else {}
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return(.Object )
}

# > @adjustBorderColor : setters ####

#' @exportMethod setAdjustBorderColor
setGeneric(name = "setAdjustBorderColor", def = function(.Object, adjustBorderColor){ standardGeneric("setAdjustBorderColor") } )
#' @title Setter for adjustBorderColor
#' @inheritParams amBalloon
#' @param .Object: Object of class \code{\linkS4class{AmBalloon}}.
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
#' @param .Object: Object of class \code{\linkS4class{AmBalloon}}.
#' @inheritParams amBalloon
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
#' @param .Object:
#' Object of class \code{\linkS4class{AmBalloon}}.
#' @inheritParams amBalloon
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
#' @param .Object:
#' Object of class \code{\linkS4class{AmBalloon}}.
#' @inheritParams amBalloon
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
#' @param .Object: \code{\linkS4class{AmBalloon}}.
#' @examples
#' library(pipeR)
#' amBalloon(adjustBorderColor = TRUE) %>>% listProperties
#' @importFrom rlist list.append
setMethod(f = "listProperties", signature = "AmBalloon",
          definition = function(.Object)
          {
            ls <- callNextMethod()
            if (length(.Object@adjustBorderColor)) {
              ls <- rlist::list.append(ls, adjustBorderColor = .Object@adjustBorderColor)
            } else {}
            if (length(.Object@color)) {
              ls <- rlist::list.append(ls, color = .Object@color)
            } else {}
            if (length(.Object@cornerRadius)) {
              ls <- rlist::list.append(ls, cornerRadius = .Object@cornerRadius)
            } else {}
            if (length(.Object@fillColor)) {
              ls <- rlist::list.append(ls, fillColor = .Object@fillColor)
            } else {}
            return (ls)
          }
)