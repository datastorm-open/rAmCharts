#' @include AmObject.R
NULL

#' @title AmBalloon class
#' @author DataKnowledge
#' @description Creates the balloons (tooltips) of the chart.
#' It follows the mouse cursor when you roll-over the data items.
#' The framework generates the instances automatically you only need to adjust the appearance to your needs.
#' @details Run \code{api("AmBalloon")} for more information and all avalaible properties.
#' @slot adjustBorderColor \code{logical}.
#' If this is set to TRUE, border color instead of background color will be changed when
#' user rolls-over the slice, graph, etc.
#' @slot color \code{character}.
#' Color of text in the balloon.
#' @slot cornerRadius \code{numeric}.
#' Balloon corner radius.
#' @slot fillColor \code{character}.
#' Balloon background color. Usually balloon background color is set by the chart.
#' Only if "adjustBorderColor" is "true" this color will be used.
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' See examples for details.
#' Inherited from \code{\linkS4class{AmObject}}.
#' @slot otherProperties \code{list},
#' containing other avalaible properties non coded in the package yet.
#' Inherited from \code{\linkS4class{AmObject}}.
#' @slot value \code{numeric}.
#' Inherited from \code{\linkS4class{AmObject}}.
#' @export
setClass(Class = "AmBalloon", contains = "AmObject",
         representation = representation(
           adjustBorderColor = "logical",
           color = "character",
           cornerRadius = "numeric",
           fillColor = "character"
         ))

#' @title Initialize an AmBalloon
#' @param .Object \linkS4class{AmBalloon}.
#' @param adjustBorderColor \code{logical}.
#' If this is set to TRUE, border color instead of background color will be changed when
#' user rolls-over the slice, graph, etc.
#' @param color \code{character}.
#' Color of text in the balloon.
#' @param cornerRadius \code{numeric}.
#' Balloon corner radius.
#' @param fillColor \code{character}.
#' Balloon background color. Usually balloon background color is set by the chart.
#' Only if "adjustBorderColor" is "true" this color will be used.
#' @param ...
#' Other properties of AmBalloon.
#' See \url{http://docs.amcharts.com/3/javascriptcharts/AmBalloon}.
#' @return An object of class \code{\linkS4class{AmBalloon}}.
#' @examples
#' new("AmBalloon", cornerRadius = 10)
#' @rdname initialize-AmBalloon
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

#' @examples
#' amBalloon(adjustBorderColor = TRUE, color = "#000000")
#' @rdname initialize-AmBalloon
#' @export
amBalloon <- function(adjustBorderColor, color, cornerRadius, fillColor, ...) {
  .Object <- new("AmBalloon")
  if (!missing(adjustBorderColor)) {
    .Object@adjustBorderColor <- adjustBorderColor
  } else {}
  if (!missing(color)) {
    .Object@color<- color
  } else {}
  if (!missing(cornerRadius)) {
    .Object@cornerRadius <- cornerRadius
  } else {}
  if (!missing(fillColor)) {
    .Object@fillColor <- fillColor
  } else {}
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return(.Object )
}

# > @adjustBorderColor : setters ####

#' @title Setter for adjustBorderColor
#' @examples
#' setAdjustBorderColor(.Object = amBalloon(), adjustBorderColor = TRUE)
#' @rdname initialize-AmBalloon
#' @export
setGeneric(name = "setAdjustBorderColor", def = function(.Object, adjustBorderColor) { standardGeneric("setAdjustBorderColor") } )
#' @rdname initialize-AmBalloon
setMethod(f = "setAdjustBorderColor", signature = c("AmBalloon", "logical"),
          definition = function(.Object, adjustBorderColor)
          {
            .Object@adjustBorderColor <- adjustBorderColor
            validObject(.Object)
            return(.Object)
          })

# > @color : setters ####

#' @title Setter for color
#' @examples
#' setColor(.Object = amBalloon(), color = "#000000")
#' @rdname initialize-AmBalloon
#' @export
setGeneric(name = "setColor", def = function(.Object, color) { standardGeneric("setColor") } )
#' @rdname initialize-AmBalloon
setMethod(f = "setColor", signature = c("AmBalloon", "character"),
          definition = function(.Object, color)
          {
            .Object@color <- color
            validObject(.Object)
            return(.Object)
          })

# > @cornerRadius : setters ####

#' @title Setter for corner radius
#' @examples
#' setCornerRadius(.Object = amBalloon(), cornerRadius = 5)
#' @rdname initialize-AmBalloon
#' @export
setGeneric(name = "setCornerRadius", def = function(.Object, cornerRadius) {standardGeneric("setCornerRadius")})
#' @rdname initialize-AmBalloon
setMethod(f = "setCornerRadius", signature = c("AmBalloon", "numeric"),
          definition = function(.Object, cornerRadius)
          {
            .Object@cornerRadius <- cornerRadius
            validObject(.Object)
            return(.Object)
          })

# > @fillColor : setters ####

#' @title Setter for fillColor
#' @examples
#' setFillColor(.Object = amBalloon(), fillColor = "#FFFFFF")
#' @rdname initialize-AmBalloon
#' @export
setGeneric(name = "setFillColor", def = function(.Object, fillColor) {standardGeneric("setFillColor")})
#' @rdname initialize-AmBalloon
setMethod(f = "setFillColor", signature = c("AmBalloon", "character"),
          definition = function(.Object, fillColor)
          {
            .Object@fillColor <- fillColor
            validObject(.Object)
            return(.Object)
          })

# listProperties ####

#' @rdname listProperties-AmObject
#' @examples
#' amBalloon(adjustBorderColor = TRUE)
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
          })