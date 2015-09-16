#' @include AmObject.R sharedGenerics.R
NULL

#' @title AmGraph class
#' @author DataKnowledge
#' @description This class represents data for a serial chart
#' 
#' @slot balloonText  \code{character}.
#' Balloon text. You can use tags like [[value]], [[description]], [[percents]], [[open]], [[category]]
#' or any other field name from your data provider. HTML tags can also be used.
#' 
#' @slot title \code{character}. Graph title.
#' 
#' @slot type \code{character}.
#' Type of the graph. Possible values are: "line", "column", "step", "smoothedLine", "candlestick", "ohlc".
#' XY and Radar charts can only display "line" otherArguments graphs.
#' 
#' @slot valueField \code{character}.
#' Name of the value field in your dataProvider.
#' 
#' @slot listeners \code{"list"} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string. See examples for details.
#' 
#' @slot otherProperties \code{"list"},
#' containing other avalaible properties non coded in the package yet
#' 
#' @slot value \code{numeric}.
#' 
#' @examples
#' \dontrun{
#' new("AmGraph")
#' unclass(new("AmGraph"))
#' }
#' @rdname AmGraph-class
#' @export
setClass (Class = "AmGraph", contains = "AmObject",
  representation = representation(
    balloonText = "character",
    title = "character",
    type = "character",
    valueField = "character"
  ),
  validity = function(object)
  {
    if (length(object@balloonText) > 1) {
      stop("[AmGraph : validation] : argument 'balloonText' must be of length 1")
    } else {}
    if (length(object@type) > 1) {
      stop("[AmGraph : validation] : argument 'type' must be of length 1")
    } else {}
    if (length(object@title) > 1) {
      stop("[AmGraph : validation] : argument 'title' must be of length 1")
    } else {}
    if (length(object@valueField) > 1) {
      stop("[AmGraph : validation] : argument 'valueField' must be of length 1")
    } else {}
    return(TRUE)
  })

#' @title Initialize an AmGraph
#' @description To create an AmGraph, you can use the usual methode
#' Initialize or the constructor.
#' You can update properties with setters.
#' @param .Object \linkS4class{AmGraph}.
#' @param animationPlayed \code{logical}.
#' @param balloonText \code{character}.
#' Balloon text. You can use tags like [[value]], [[description]], [[percents]], [[open]], [[category]]
#' or any other field name from your data provider. HTML tags can also be used.
#' @param title \code{character}. Graph title.
#' @param type \code{character}.
#' Type of the graph. Possible values are: "line", "column", "step", "smoothedLine", "candlestick", "ohlc".
#' XY and Radar charts can only display "line" otherArguments graphs.
#' @param valueField \code{character}.
#' Name of the value field in your dataProvider.
#' @param ... Other properties.
#' @return An object of class \linkS4class{AmGraph} with the properties given.
#' @examples
#' new("AmGraph", valueField = "value")
#' @rdname initialize-AmGraph
#' @export
setMethod(f = "initialize", signature = "AmGraph",
          definition = function(.Object, animationPlayed = FALSE, balloonText,
                                title, type, valueField, ...)
          {  
            if (!missing(balloonText)) {
              .Object@balloonText <- balloonText
            } else {}
            if (!missing(title)) {
              .Object@title <- title
            } else {}
            if (!missing(type)) {
              .Object@type <- type
            } else {}
            if (!missing(valueField)) {
              .Object@valueField <- valueField
            } else {}
            .Object@otherProperties <- list (animationPlayed = animationPlayed, ...)
            validObject(.Object)
            return(.Object)
          })

#' @description Constructor
#' @examples
#' amGraph(balloonText = "balloonText", "type" = "column",
#'         valueField = "value", animationPlayed = TRUE)
#' @describeIn initialize-AmGraph
#' @export
amGraph <- function(animationPlayed = FALSE, balloonText, title, type, valueField, ...)
{
  .Object <- new(Class="AmGraph", animationPlayed = animationPlayed)
  if (!missing(balloonText)) {
    .Object@balloonText <- balloonText
  } else {}
  if (!missing(title)) {
    .Object@title <- title
  } else {}
  if (!missing(type)) {
    .Object@type <- type
  } else {}
  if (!missing(valueField)) {
    .Object@valueField <- valueField
  } else {}
  .Object@otherProperties <- list(...)
  validObject(.Object)
  return(.Object)
}

#' @title Constructor for a stockGraph (class AmGraph)
#' @param animationPlayed \code{logical}.
#' @param balloonText \code{character}.
#' Balloon text. You can use tags like [[value]], [[description]], [[percents]], [[open]], [[category]]
#' or any other field name from your data provider. HTML tags can also be used.
#' @param title \code{character}. Graph title.
#' @param type \code{character}.
#' Type of the graph. Possible values are: "line", "column", "step", "smoothedLine", "candlestick", "ohlc".
#' XY and Radar charts can only display "line" otherArguments graphs.
#' @param valueField \code{character}.
#' Name of the value field in your dataProvider.
#' @param ... Other properties
#' @examples
#' stockGraph(balloonText = "balloonText", "type" = "column",
#'            valueField = "value", animationPlayed = TRUE)
#' @return An object of class \code{\linkS4class{AmGraph}}.
#' @export 
stockGraph <- function(animationPlayed = FALSE, balloonText, title, type, valueField, ...)
{
  amGraph(animationPlayed, balloonText, title, type, valueField, ...)
}

# > @balloonText ####

#' @description Setter for balloonText.
#' @examples
#' setBalloonText(.Object = amGraph(), balloonText = "performance")
#' @rdname initialize-AmGraph
#' @export
setGeneric(name = "setBalloonText", def = function(.Object, balloonText) {standardGeneric("setBalloonText")})
#' @rdname initialize-AmGraph
setMethod(f = "setBalloonText", signature = c("AmGraph", "character"),
          definition = function(.Object, balloonText)
          {
            .Object@balloonText <- balloonText
            validObject(.Object)
            return(.Object)
          })

# > @title ####

#' @description Setter for title.
#' @examples
#' setTitle(.Object = amGraph(), title = "Power")
#' @rdname initialize-AmGraph
setMethod(f = "setTitle", signature = c("AmGraph", "character"),
          definition = function(.Object, title)
          {
            .Object@title <- title
            validObject(.Object)
            return(.Object)
          })

#' @description Setter for type.
#' @examples
#' setType(.Object = amGraph(), type = "type")
#' @rdname initialize-AmGraph
setMethod(f = "setType", signature = c("AmGraph", "character"),
          definition = function(.Object, type)
          {
            .Object@type <- type
            validObject(.Object)
            return(.Object)
          })

# > @valueField ####

#' @description Setter for valueField
#' @examples
#' setValueField(.Object = amGraph(), valueField = "score")
#' @rdname initialize-AmGraph
#' @export
setGeneric(name = "setValueField", def = function(.Object, valueField) {standardGeneric("setValueField")})
#' @rdname initialize-AmGraph
setMethod(f = "setValueField", signature = c("AmGraph", "character"),
          definition = function(.Object, valueField)
          {
            .Object@valueField <- valueField
            validObject(.Object)
            return(.Object)
          })

# > METHODS ###

# listProperties ####
#' @description Attributes of an AmGraph object.
#' @examples
#' listProperties(amGraph(balloonText = "toto", type = "type", valueField = "valueField"))
#' listProperties(amGraph(balloonText = "toto", type = "type"))
#' @rdname listProperties-AmObject
setMethod(f = "listProperties", signature = "AmGraph",
          definition = function(.Object)
          {
            ls <- callNextMethod()
            if (length(.Object@balloonText)) {
              ls <- rlist::list.append(ls, balloonText = .Object@balloonText)
            } else {}
            if (length(.Object@title)) {
              ls <- rlist::list.append(ls, title = .Object@title)
            } else {}
            if (length(.Object@type)) {
              ls <- rlist::list.append(ls, type = .Object@type)
            } else {}
            if (length(.Object@valueField)) {
              ls <- rlist::list.append(ls, valueField = .Object@valueField)
            } else {}
            return (ls)
          })