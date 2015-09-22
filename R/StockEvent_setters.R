#' @include classUnion.R
NULL

#' @rdname initialize-StockEvent
#' @export
setGeneric(name = "setStockGraph", def = function(.Object, stockGraph = NULL, ...) {standardGeneric("setStockGraph")})
#' @examples
#' setStockGraph(.Object = stockEvent(), id = "stockGraph1", balloonText = "balloonText")
#' # equivalent to:
#' stockGraph_obj <- stockGraph(id = "stockGraph1", balloonText = "balloonText")
#' setStockGraph(.Object = stockEvent(), stockGraph = stockGraph_obj)
#' # if stockGraph_obj has already been added to the chart:
#' setStockGraph(.Object = stockEvent(), stockGraph = "stockGraph1")
#' @rdname initialize-StockEvent
setMethod(f = "setStockGraph", signature = c("StockEvent", "AmGraphOrCharacterOrMissing"),
          definition = function(.Object, stockGraph = NULL, ...)
          {
            if (is.null(stockGraph) && !missing(...)) {
              stockGraph <- stockGraph(...)
            } else if (is.null(stockGraph) && missing(...)) {
              stop("You must either give argument 'stockGraph' or its properties...")
            } else {}
            
            if (is(stockGraph, "AmGraph")) {
              .Object@stockGraph <- listProperties(stockGraph)
            } else if (length(stockGraph) == 1) {
              .Object@stockGraph <- stockGraph
            } else {
              stop("Argument 'stockGraph' non valid")
            }
            
            validObject(.Object)
            return(.Object)
          })