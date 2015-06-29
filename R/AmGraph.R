#' @include AmObject.R sharedGenerics.R
NULL

#' @title AmGraph class
#' @author DataKnowledge
#' @description This class represents data for a serial chart
#' @section Slots:
#' @slot \code{balloonText}: {Object of class \code{character}.
#' Balloon text. You can use tags like [[value]], [[description]], [[percents]], [[open]], [[category]]
#' or any other field name from your data provider. HTML tags can also be used.}
#' @slot \code{title}: {Object of class \code{character}. Graph title.}
#' @slot \code{type}: {Object of class \code{character}.
#' Type of the graph. Possible values are: "line", "column", "step", "smoothedLine", "candlestick", "ohlc".
#' XY and Radar charts can only display "line" otherArguments graphs.}
#' @slot \code{valueField}: {Object of class \code{character}.
#' Name of the value field in your dataProvider.}
#' @examples
#' \dontrun{
#' new("AmGraph")
#' unclass(new("AmGraph"))
#' }
#' @family rAmChart classes
#' @export
setClass( Class = "AmGraph", contains = "AmObject",
  representation = representation(
    balloonText = "character",
    title = "character",
    type = "character",
    valueField = "character"
  ),
  validity = function(object)
  {
    if(length(object@balloonText) > 1){
      stop("[AmGraph : validation] : argument 'balloonText' must be of length 1")
    }else{}
    if(length(object@type) > 1){
      stop("[AmGraph : validation] : argument 'type' must be of length 1")
    }else{}
    if(length(object@title) > 1){
      stop("[AmGraph : validation] : argument 'title' must be of length 1")
    }else{}
    if(length(object@valueField) > 1){
      stop("[AmGraph : validation] : argument 'valueField' must be of length 1")
    }else{}
    return(TRUE)
  }
)

#' @title Initialize an AmGraph
#' @examples
#' new("AmGraph", valueField = "value")
#' @export
setMethod(f = "initialize", signature = "AmGraph",
          definition = function(.Object, animationPlayed = FALSE, balloonText, title, type, valueField, ...)
          {  
            if(!missing(balloonText)){
              .Object@balloonText <- balloonText
            }else{}
            if(!missing(title)){
              .Object@title <- title
            }else{}
            if(!missing(type)){
              .Object@type <- type
            }else{}
            if(!missing(valueField)){
              .Object@valueField <- valueField
            }else{}
            .Object@otherProperties <- list( animationPlayed = animationPlayed, ...)
            validObject(.Object)
            return(.Object)
          }
)

#’ Constructor.
#' @title Constructor for an AmGraph
#' @param \code{balloonText}: {Object of class \code{character}.
#' Balloon text. You can use tags like [[value]], [[description]], [[percents]], [[open]], [[category]]
#' or any other field name from your data provider. HTML tags can also be used.}
#' @param \code{title}: {Object of class \code{character}. Graph title.}
#' @param \code{type}: {Object of class \code{character}.
#' Type of the graph. Possible values are: "line", "column", "step", "smoothedLine", "candlestick", "ohlc".
#' XY and Radar charts can only display "line" otherArguments graphs.}
#' @param \code{valueField}: {Object of class \code{character}.
#' Name of the value field in your dataProvider.}
#' @return An \code{\linkS4class{AmGraph}} object
#' @examples
#' amGraph(balloonText = "balloonText", "type" = "column", valueField = "value", animationPlayed = TRUE)
#' @export
amGraph <- function(animationPlayed = FALSE, balloonText, title, type, valueField, ...)
{
  .Object <- new(Class="AmGraph", animationPlayed = animationPlayed)
  if(!missing(balloonText)){
    .Object@balloonText <- balloonText
  }else{}
  if(!missing(title)){
    .Object@title <- title
  }else{}
  if(!missing(type)){
    .Object@type <- type
  }else{}
  if(!missing(valueField)){
    .Object@valueField <- valueField
  }else{}
  .Object@otherProperties <- list(...)
  validObject(.Object)
  return(.Object)
}

#’ Constructor for stockGraph.
#' @title Constructor for a stockGraph (class AmGraph)
#' @param \code{balloonText}: {Object of class \code{character}.
#' Balloon text. You can use tags like [[value]], [[description]], [[percents]], [[open]], [[category]]
#' or any other field name from your data provider. HTML tags can also be used.}
#' @param \code{title}: {Object of class \code{character}. Graph title.}
#' @param \code{type}: {Object of class \code{character}.
#' Type of the graph. Possible values are: "line", "column", "step", "smoothedLine", "candlestick", "ohlc".
#' XY and Radar charts can only display "line" otherArguments graphs.}
#' @param \code{valueField}: {Object of class \code{character}.
#' Name of the value field in your dataProvider.}
#' @param \code{...}: See \code{\url{http://docs.amcharts.com/3/javascriptstockchart/StockGraph}}
#' @return An \code{\linkS4class{AmGraph}} object
#' @examples
#' stockGraph(balloonText = "balloonText", "type" = "column", valueField = "value", animationPlayed = TRUE)
#' @export 
stockGraph <- function(animationPlayed = FALSE, balloonText, title, type, valueField, ...)
{
  amGraph(animationPlayed, balloonText, title, type, valueField, ...)
}

# > SETTERS ####

# > @balloonText ####
#' @exportMethod setBalloonText
setGeneric(name = "setBalloonText", def = function(.Object, balloonText){standardGeneric("setBalloonText")})
#' @title Setter
#' @examples
#' amGraph() %>>% setBalloonText("balloonText")
#' @name setBalloonText
#' @rdname setBalloonText
#' @export
setMethod(f = "setBalloonText", signature = c("AmGraph", "character"),
          definition = function(.Object, balloonText)
          {
            .Object@balloonText <- balloonText
            validObject(.Object)
            return(.Object)
          }
)

# > @title ####

#' @title Setter
#' @examples
#' amGraph() %>>% setTitle("title")
#' @rdname setTitle
#' @name setTitle
#' @export
setMethod(f = "setTitle", signature = c("AmGraph", "character"),
          definition = function(.Object, title)
          {
            .Object@title <- title
            validObject(.Object)
            return(.Object)
          }
)

#' @title Setter
#' @examples
#' amGraph() %>>% setType("type")
#' @name setType
#' @rdname setType
#' @export
setMethod(f = "setType", signature = c("AmGraph", "character"),
          definition = function(.Object, type)
          {
            .Object@type <- type
            validObject(.Object)
            return(.Object)
          }
)

# > @valueField ####

#' @exportMethod setValueField
setGeneric(name = "setValueField", def = function(.Object, valueField){standardGeneric("setValueField")})
#' @title Setter
#' @examples
#' amGraph() %>>% setValueField("valueField")
#' @name setValueField
#' @rdname setValueField
#' @export
setMethod(f = "setValueField", signature = c("AmGraph", "character"),
          definition = function(.Object, valueField)
          {
            .Object@valueField <- valueField
            validObject(.Object)
            return(.Object)
          }
)

# > METHODS ###

# listProperties ####
#' @title List attributes of an AmGraph object
#' @description This function is used to list attributes before addind to graphs (attribute of AmChart)
#' @examples
#' amGraph(balloonText = "toto", type = "type", valueField = "valueField") %>>%
#' listProperties
#' 
#' amGraph(balloonText = "toto", type = "type") %>>%
#' listProperties
setMethod(f = "listProperties", signature = "AmGraph",
          definition = function(.Object)
          {
            ls <- callNextMethod()
            if( length(.Object@balloonText) > 0 ){
              ls <- ls %>>% list.append(balloonText = .Object@balloonText)
            }else{}
            if( length(.Object@title) > 0 ){
              ls <- ls %>>% list.append(title = .Object@title)
            }else{}
            if( length(.Object@type) > 0 ){
              ls <- ls %>>% list.append(type = .Object@type)
            }else{}
            if( length(.Object@valueField) > 0 ){
              ls <- ls %>>% list.append(valueField = .Object@valueField)
            }else{}
            return (ls)
          }
)