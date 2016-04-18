#' @include sharedGenerics.R classUnion.R
NULL

#' @examples
#' # chartScrollbar with default graph
#' setGraph(.Object = chartScrollbar())
#' 
#' # example with arguments
#' setGraph(.Object = chartScrollbar(), id = "graph1", balloonText = "performance")
#' # equivalent to:
#' graph_obj <- amGraph(id = "graph1", balloonText = "performance")
#' (chartScrollbar_obj <- setGraph(.Object = chartScrollbar(), graph = graph_obj))
#' # or, iff graph_obj has alreadey been added to the chart:
#' setGraph(.Object = chartScrollbar(), graph = "graph1")
#' \dontshow{
#' print(chartScrollbar_obj)
#' }
#' # ---
#' @rdname initialize-ChartScrollbar
setMethod(f = "setGraph", signature = c("ChartScrollbar", "AmGraphOrCharacterOrMissing"),
          definition = function(.Object, graph = NULL, ...)
          {
            if (is.null(graph) && !missing(...)) {
              graph <- amGraph(...)
            } else if (is.null(graph) && missing(...)) {
              message("default graph added to chartScrollbar")
              graph <- amGraph(balloonText = "[[value]]")
            }
            
            if (is(graph, "AmGraph")) {
              .Object@graph <- listProperties(graph)
            } else if (length(graph) == 1) {
              .Object@graph <- graph
            } else {
              stop("Argument 'graph' non valid")
            }
            
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' setEnabled(.Object = chartScrollbar(), enabled = TRUE)
#' @rdname initialize-ChartScrollbar
#' @export
setGeneric(name = "setEnabled", def = function(.Object, enabled) {standardGeneric("setEnabled")})
#' @rdname initialize-ChartScrollbar
setMethod(f = "setEnabled", signature = c("ChartScrollbar", "logical"),
          definition = function(.Object, enabled)
          {
            .Object@enabled <- enabled
            validObject(.Object)
            return(.Object)
          })
