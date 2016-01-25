#' @include class_AmObject.R utils_sharedGenerics.R
NULL

#' @title AmGraph class
#' @description Creates the visualization of the data in following types:
#' line, column, step line, smoothed line, olhc and candlestick.
#' 
#' @slot balloonText  \code{character}.
#' Balloon text. You can use tags like [[value]], [[description]], [[percents]], [[open]], [[category]]
#' or any other field name from your data provider. HTML tags can also be used.
#' @slot title \code{character}. Graph title.
#' @slot type \code{character}.
#' Type of the graph. Possible values are: "line", "column", "step", "smoothedLine", "candlestick", "ohlc".
#' XY and Radar charts can only display "line" otherArguments graphs.
#' @slot valueField \code{character}.
#' Name of the value field in your dataProvider.
#' @slot listeners \code{"list"} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' See examples for details.
#' @slot otherProperties \code{"list"},
#' containing other avalaible properties non coded in the package yet
#' @slot value \code{numeric}.
#' 
#' @details Run \code{api("AmGraph")} for more details and all avalaible properties.
#' 
#' 
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
#' 
#' @return An object of class \linkS4class{AmGraph} with the properties given.
#' 
#' @examples
#' # --- method 'initialize'
#' new("AmGraph", valueField = "value")
#' 
#' @rdname AmGraph
#' @export
#' 
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

#' @rdname AmGraph
#' 
#' @examples
#' # constructor
#' amGraph(balloonText = "balloonText", "type" = "column",
#'         valueField = "value", animationPlayed = TRUE)
#' @export
#' 
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

#' @rdname AmGraph
#' @examples
#' # --- shortcut constructor
#' graph(balloonText = "balloonText", "type" = "column",
#'       valueField = "value", animationPlayed = TRUE)
#'       
#' @export
#' 
graph <- function(animationPlayed = FALSE, balloonText, title, type, valueField, ...)
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
#' @description Constructor used for \linkS4class{AmStockChart}
#' 
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
#' 
#' @return An object of class \linkS4class{AmGraph}.
#' 
#' @examples
#' # --- constructor
#' stockGraph(balloonText = "balloonText", "type" = "column",
#'            valueField = "value", animationPlayed = TRUE)
#'            
#' @export 
stockGraph <- function(animationPlayed = FALSE, balloonText, title, type, valueField, ...)
{
  amGraph(animationPlayed, balloonText, title, type, valueField, ...)
}

# > @balloonText ####

#' @rdname AmGraph
#' @export
#' 
setGeneric(name = "setBalloonText", def = function(.Object, balloonText) {standardGeneric("setBalloonText")})
#' @rdname AmGraph
#' @examples
#' # --- update 'balloonText'
#' setBalloonText(.Object = amGraph(), balloonText = "performance")
#' 
setMethod(f = "setBalloonText", signature = c("AmGraph", "character"),
          definition = function(.Object, balloonText)
          {
            .Object@balloonText <- balloonText
            validObject(.Object)
            return(.Object)
          })

# > @title ####

#' @rdname AmGraph
#' @examples
#' # --- update 'title'
#' setTitle(.Object = amGraph(), title = "Power")
#'
setMethod(f = "setTitle", signature = c("AmGraph", "character"),
          definition = function(.Object, title)
          {
            .Object@title <- title
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' # --- update 'type'
#' setType(.Object = amGraph(), type = "type")
#' 
#' @rdname AmGraph
setMethod(f = "setType", signature = c("AmGraph", "character"),
          definition = function(.Object, type)
          {
            .Object@type <- type
            validObject(.Object)
            return(.Object)
          })

# > @valueField ####

#' @rdname AmGraph
#' @export
#' 
setGeneric(name = "setValueField", def = function(.Object, valueField) {standardGeneric("setValueField")})
#' @rdname AmGraph
#' @examples
#' # --- update valueField
#' setValueField(.Object = amGraph(), valueField = "score")
#' 
setMethod(f = "setValueField", signature = c("AmGraph", "character"),
          definition = function(.Object, valueField)
          {
            .Object@valueField <- valueField
            validObject(.Object)
            return(.Object)
          })

# > METHODS ###

# listProperties ####

#' @rdname listProperties-AmObject
#' @examples
#' # --- signature 'AmGraph'
#' listProperties(amGraph(balloonText = "toto", type = "type", valueField = "valueField"))
#' listProperties(amGraph(balloonText = "toto", type = "type"))
#' 
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