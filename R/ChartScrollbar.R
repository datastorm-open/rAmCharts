#' @include AmObject.R
NULL

#' @title ChartScrollbar class
#' @author DataKnowledge
#' 
#' @slot updateOnReleaseOnly
#' Object of class \code{logical}.
#' Specifies if the chart should be updated while dragging/resizing the scrollbar
#' or only at the moment when user releases mouse button.
#' 
#' @slot graph
#' Object of class \code{list}.
#' Specifies which graph properties will be displayed in the scrollbar.
#' Only Serial chart's scrollbar can display a graph.
#' 
#' @slot listeners
#' Object of class \code{"list"} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' See examples for details.
#' 
#' @slot otherProperties
#' Object of class \code{"list"},
#' containing other avalaible properties non coded in the package yet.
#' 
#' @slot value
#' Object of class \code{numeric}.
#' 
#' @export
setClass( Class = "ChartScrollbar", contains = "AmObject",
          representation = representation(graph = "list", updateOnReleaseOnly = "logical" )
)

#' @title Initialize a ChartScrollbar
#' @param .Object \linkS4class{ChartScrollbar}.
#' @param graph \linkS4class{AmGraph}.
#' Specifies which graph will be displayed in the scrollbar.
#' @param pathToImages \code{character} URL to API images (for buttons)
#' @param updateOnReleaseOnly \code{logical}.
#' Specifies if the chart should be updated while dragging/resizing the scrollbar
#' or only at the moment when user releases mouse button.
#' @param ... Other preperties
#' See \url{http://docs.amcharts.com/3/javascriptcharts/ChartScrollbar}
#' @examples
#' new("ChartScrollbar", graph = "g1")
#' new("ChartScrollbar", graph = amGraph(test = 1))
#' @rdname initialize-ChartScrollbar
#' @export
setMethod(f = "initialize", signature = "ChartScrollbar",
          definition = function(.Object,
                                graph,
                                pathToImages = "http://www.amcharts.com/lib/3/images/",
                                updateOnReleaseOnly, ...)
          { 
            if (!missing(graph)) {
              .Object <- setGraph(.Object, graph = graph)
            } else {}
            if(!missing(updateOnReleaseOnly)){
              .Object@updateOnReleaseOnly <- updateOnReleaseOnly
            } else {}
            .Object <- setProperties(.Object, pathToImages = pathToImages,...)
            validObject(.Object)
            return(.Object)
          })


#' @describeIn initialize-ChartScrollbar
#' @examples
#' chartScrollbar()
#' chartScrollbar(updateOnReleaseOnly = TRUE)
#' @export
chartScrollbar <- function(graph,
                           pathToImages = "http://www.amcharts.com/lib/3/images/",
                           updateOnReleaseOnly,...){
  .Object <- new("ChartScrollbar", pathToImages = pathToImages)
  if (!missing(graph)) {
    .Object <- setGraph(.Object, graph)
  } else {}
  if (!missing(updateOnReleaseOnly)) {
    .Object@updateOnReleaseOnly <- updateOnReleaseOnly
  } else {}
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return( .Object )
}

# > @graph : setters ####

#' @examples
#' setGraph(.Object = chartScrollbar(), test = 1)
#' @rdname initialize-ChartScrollbar
#' @export
setMethod(f = "setGraph", signature = c("ChartScrollbar"),
           definition = function(.Object, graph = NULL, ...)
           {
             if (is.null(graph) && !missing(...)) {
               .Object@otherProperties <- rlist::list.append(
                 .Object@otherProperties,  graph = listProperties(amGraph(...))
               )
             } else if (is.null(graph) && missing(...)) {
               .Object@otherProperties <- rlist::list.append(
                 .Object@otherProperties,  graph = listProperties(amGraph(balloonText = "[[value]]"))
               )
             } else if (!is.null(graph) && is(graph, "AmGraph")) {
               .Object@otherProperties <- rlist::list.append(
                 .Object@otherProperties,  graph = listProperties(graph)
               )
             } else if (!is.null(graph) && is(graph, "character")) {
               .Object@otherProperties <- rlist::list.append(
                 .Object@otherProperties,  graph = graph
               )
             } else {}
             validObject(.Object)
             return(.Object)
           })

# > @updateOnReleaseOnly : setters ####

#' @rdname initialize-ChartScrollbar
#' @export
setGeneric(name = "setUpdateOnReleaseOnly",
           def = function(.Object, updateOnReleaseOnly){ standardGeneric("setUpdateOnReleaseOnly") } )
#' @examples
#' setUpdateOnReleaseOnly(.Object = chartScrollbar(), updateOnReleaseOnly = TRUE)
#' @rdname initialize-ChartScrollbar
#' @export
setMethod(f = "setUpdateOnReleaseOnly", signature = c("ChartScrollbar", "logical"),
          definition = function(.Object, updateOnReleaseOnly)
          {
            .Object@updateOnReleaseOnly <- updateOnReleaseOnly
            validObject(.Object)
            return(.Object)
          })

#' @rdname listProperties-AmObject
#' @examples
#' listProperties(chartScrollbar(updateOnReleaseOnly = TRUE))
setMethod( f = "listProperties", signature = "ChartScrollbar",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length( .Object@updateOnReleaseOnly)) {
               ls <- rlist::list.append(ls, updateOnReleaseOnly = .Object@updateOnReleaseOnly)
             } else {}
             return(ls)
           })