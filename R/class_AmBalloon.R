#' @include class_AmObject.R
NULL

#' @title AmBalloon class
#' @description Creates the balloons (tooltips) of the chart.
#' It follows the mouse cursor when you roll-over the data items.
#' The framework generates the instances automatically, you just have to adjust
#' the appearance to your needs.
#' 
#' @slot adjustBorderColor \code{logical}.
#' If TRUE, border color will be changed when user rolls-over the slice, graph, 
#' etc, instead of background color.
#' @slot color \code{character}.
#' Balloon text color.
#' @slot cornerRadius \code{numeric}.
#' Balloon corner radius.
#' @slot fillColor \code{character}.
#' Balloon background color. It is usually defined by the chart itself. If
#' "adjustBorderColor" is set to TRUE, the balloon background color will be
#' equal to "fillColor".
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must be a character string.
#' See examples for details.
#' Inherited from \linkS4class{AmObject}.
#' @slot otherProperties \code{list}
#' containing other avalaible properties not yet implemented in the package.
#' Inherited from \linkS4class{AmObject}.
#' @slot value \code{numeric}.
#' Inherited from \linkS4class{AmObject}.
#' 
#' @details Run \code{api("AmBalloon")} for more information and all avalaible properties.
#' 
setClass(Class = "AmBalloon", contains = "AmObject",
         representation = representation(
           adjustBorderColor = "logical",
           color = "character",
           cornerRadius = "numeric",
           fillColor = "character"
         ))

#' @title Initializes an AmBalloon
#' @description Initializes or updates an object \linkS4class{AmBalloon}.
#' 
#' @param .Object \linkS4class{AmBalloon}.
#' @param adjustBorderColor \code{logical}, 
#' if TRUE, border color will be changed when user rolls-over the slice, graph, 
#' etc, instead of background color.
#' @param color \code{character}, 
#' balloon text color.
#' @param cornerRadius \code{numeric}, 
#' balloon corner radius.
#' @param fillColor \code{character}, 
#' balloon background color. It is usually defined by the chart itself. If
#' "adjustBorderColor" is set to TRUE, the balloon background color will be
#' equal to "fillColor".
#' @param ... other properties of AmBalloon.
#' See \url{https://docs.amcharts.com/3/javascriptcharts/AmBalloon}.
#' 
#' @return An object, possibly updated,  of class \linkS4class{AmBalloon}.
#' 
#' @examples
#' 
#' new("AmBalloon", cornerRadius = 10)
#' 
#' @rdname AmBalloon
#' @export
#' 
setMethod(f = "initialize", signature = "AmBalloon",
          definition = function(.Object, adjustBorderColor, color, cornerRadius, fillColor, ...)
          {  
            if (!missing(adjustBorderColor)) .Object <- setAdjustBorderColor(.Object = .Object, adjustBorderColor = adjustBorderColor)
            if (!missing(color)) .Object <- setColor(.Object = .Object, color = color)
            if (!missing(cornerRadius)) .Object <- setCornerRadius(.Object = .Object, cornerRadius = cornerRadius)
            if (!missing(fillColor)) .Object <- setFillColor(.Object = .Object, fillColor = fillColor)
            
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####

#' @rdname AmBalloon
#' 
#' @examples
#' amBalloon(adjustBorderColor = TRUE, color = "#000000", other = TRUE)
#' 
#' @export
amBalloon <- function(adjustBorderColor, color, cornerRadius, fillColor, ...) {
  .Object <- new("AmBalloon", ...)
  if (!missing(adjustBorderColor)) .Object@adjustBorderColor <- adjustBorderColor
  if (!missing(color)) .Object@color<- color
  if (!missing(cornerRadius)) .Object@cornerRadius <- cornerRadius
  if (!missing(fillColor)) .Object@fillColor <- fillColor

  validObject(.Object)
  return(.Object )
}

# > @adjustBorderColor : setters ####


#' @rdname AmBalloon
#' 
#' @examples
#' setAdjustBorderColor(.Object = amBalloon(), adjustBorderColor = TRUE)
#' 
#' @export
#' 
setGeneric(name = "setAdjustBorderColor", def = function(.Object, adjustBorderColor) {standardGeneric("setAdjustBorderColor")})
#' @rdname AmBalloon
setMethod(f = "setAdjustBorderColor", signature = c("AmBalloon", "logical"),
          definition = function(.Object, adjustBorderColor)
          {
            .Object@adjustBorderColor <- adjustBorderColor
            validObject(.Object)
            return(.Object)
          })

# > @color : setters ####

#' @rdname AmBalloon
#' 
#' @examples
#' setColor(.Object = amBalloon(), color = "#000000")
#' 
#' @export
#' 
setGeneric(name = "setColor", def = function(.Object, color) { standardGeneric("setColor") } )
#' @rdname AmBalloon
setMethod(f = "setColor", signature = c("AmBalloon", "character"),
          definition = function(.Object, color)
          {
            .Object@color <- color
            validObject(.Object)
            return(.Object)
          })

# > @cornerRadius : setters ####

#' @rdname AmBalloon
#' 
#' @examples
#' setCornerRadius(.Object = amBalloon(), cornerRadius = 5)
#' 
#' @export
#' 
setGeneric(name = "setCornerRadius", def = function(.Object, cornerRadius) {standardGeneric("setCornerRadius")})
#' @rdname AmBalloon
setMethod(f = "setCornerRadius", signature = c("AmBalloon", "numeric"),
          definition = function(.Object, cornerRadius)
          {
            .Object@cornerRadius <- cornerRadius
            validObject(.Object)
            return(.Object)
          })

# > @fillColor : setters ####

#' @rdname AmBalloon
#' 
#' @examples
#' setFillColor(.Object = amBalloon(), fillColor = "#FFFFFF")
#' 
#' @export
#' 
setGeneric(name = "setFillColor", def = function(.Object, fillColor) {standardGeneric("setFillColor")})
#' @rdname AmBalloon
setMethod(f = "setFillColor", signature = c("AmBalloon", "character"),
          definition = function(.Object, fillColor)
          {
            .Object@fillColor <- fillColor
            validObject(.Object)
            return(.Object)
          })
