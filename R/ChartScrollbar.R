#' @include AmObject.R
NULL

#' @title ChartScrollbar class
#' @author DataKnowledge
#' @section Slots:
#' @slot \code{updateOnReleaseOnly}: Object of class \code{logical}.
#' Specifies if the chart should be updated while dragging/resizing the scrollbar
#' or only at the moment when user releases mouse button.
#' 
#' @slot \code{graph}: Object of class \code{AmGraph}.
#' Specifies which graph will be displayed in the scrollbar.
#' 
#' @export
setClass( Class = "ChartScrollbar", contains = "AmObject",
          representation = representation( updateOnReleaseOnly = "logical" )
)

#' @title Initialize a ChartScrollbar
#' @examples
#' new("ChartScrollbar", graph = "g1")
#' new("ChartScrollbar", graph = amGraph(test = 1))
#' @export
setMethod(f = "initialize", signature = "ChartScrollbar",
          definition = function(.Object, graph, pathToImages = "http://www.amcharts.com/lib/3/images/",
                                updateOnReleaseOnly, ...)
          { 
            if(!missing(graph)){
              .Object <- setGraph(.Object, graph = graph)
            }
            if(!missing(updateOnReleaseOnly)){
              .Object@updateOnReleaseOnly <- updateOnReleaseOnly
            }
            .Object <- setProperties(.Object, pathToImages = pathToImages,...)
            validObject(.Object)
            return(.Object)
          }
)

# CONSTRUCTOR ####
#' @title
#' #’ Constructor.
#' @title Constructor for an AmGraph
#' @param \code{...}: {Properties of ChartScrollbar.
#' See \code{\url{http://docs.amcharts.com/3/javascriptcharts/ChartScrollbar}}}
#' @return An \code{\linkS4class{ChartScrollbar}} object
#' @examples
#' chartScrollbar()
#' chartScrollbar(updateOnReleaseOnly = TRUE)
#' @export
chartScrollbar <- function(graph, updateOnReleaseOnly,
                           pathToImages = "http://www.amcharts.com/lib/3/images/", ...){
  .Object <- new("ChartScrollbar", pathToImages = pathToImages)
  if(!missing(graph)){
    .Object <- setGraph(.Object, graph)
  }
  if(!missing(updateOnReleaseOnly)){
    .Object@updateOnReleaseOnly <- updateOnReleaseOnly
  }
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return( .Object )
}

# > @graph : setters ####

#' @title Setter for graph
#' @details Method to use in case of AmChart of type \code{gantt}.
#' For other type see \code{\link{setGraphs}} or \code{\link{addGraph}}.
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{graph}: (optionnal) Object of class \code{\linkS4class{AmGraph}}.
#' @return The updated object of class \code{\linkS4class{ChartScrollbar}}.
#' @examples
#' library(pipeR)
#' chartScrollbar() %>>% setGraph(test = 1)
#' @family ChartScrollbar setters
#' @family ChartScrollbar methods
#' @seealso \code{\linkS4class{ChartScrollbar}} S4 class
#' @name setGraph
#' @rdname setGraph
#' @export
setMethod( f = "setGraph", signature = c("ChartScrollbar"),
           definition = function(.Object, graph = NULL, ...)
           {
             if( is.null(graph) && !missing(...) ){
               .Object@otherProperties <- rlist::list.append(
                 .Object@otherProperties,  graph = listProperties( amGraph(...) )
               )
             }else if( is.null(graph) && missing(...) ){
               .Object@otherProperties <- rlist::list.append(
                 .Object@otherProperties,  graph = listProperties( amGraph(balloonText = "[[value]]") )
               )
             }else if( ! is.null(graph) && is(graph, "AmGraph")){
               .Object@otherProperties <- rlist::list.append(
                 .Object@otherProperties,  graph = listProperties( graph )
               )
             }else if( ! is.null(graph) && is(graph, "character") ){
               .Object@otherProperties <- rlist::list.append(
                 .Object@otherProperties,  graph = graph
               )
             }else{}
             validObject(.Object)
             return(.Object)
           }
)

# > @updateOnReleaseOnly : setters ####

#' @exportMethod setUpdateOnReleaseOnly
setGeneric(name = "setUpdateOnReleaseOnly",
           def = function(.Object, updateOnReleaseOnly){ standardGeneric("setUpdateOnReleaseOnly") } )
#' @title SETTER
#' @examples
#' library(pipeR)
#' chartScrollbar() %>>% setUpdateOnReleaseOnly(TRUE)
#' @rdname setUpdateOnReleaseOnly
#' @export
setMethod(
  f = "setUpdateOnReleaseOnly",
  signature = c("ChartScrollbar", "logical"),
  definition = function(.Object, updateOnReleaseOnly)
  {
    .Object@updateOnReleaseOnly <- updateOnReleaseOnly
    validObject(.Object)
    return(.Object)
  }
)

#' @title List properties
#' @examples
#' new("ChartScrollbar", updateOnReleaseOnly = TRUE)
#' @return Properties of the object in a list
setMethod( f = "listProperties", signature = "ChartScrollbar",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length( .Object@updateOnReleaseOnly ) > 0 ){
               ls <- list.append(ls, updateOnReleaseOnly = .Object@updateOnReleaseOnly)
             }else{}
             return(ls)
           }
)