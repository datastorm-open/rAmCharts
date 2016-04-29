#' @include classUnions.R
NULL

#' @examples
#' valueAxis_obj <- valueAxis(id = "valueAxis1")
#' setDrawOnAxis(.Object = stockPanel(), valueAxis = valueAxis_obj)
#' setDrawOnAxis(.Object = stockPanel(), valueAxis = "valueAxis1")
#' # ---
#' @rdname initialize-StockPanel
#' @export
setGeneric(name = "setDrawOnAxis",
           def = function(.Object, valueAxis = NULL, ...){ standardGeneric("setDrawOnAxis") })
#' @rdname initialize-StockPanel
setMethod(f = "setDrawOnAxis", signature = c("StockPanel", "ValueAxisOrCharacterOrMissing"),
          definition = function(.Object, valueAxis = NULL, ...)
          {
            if (is.null(valueAxis) && !missing(...)) {
              valueAxis <- valueAxis(...)
            } else if (is.null(valueAxis) && missing(...)) {
              stop("You must provide either argument 'valueAxis' or its properties")
            } else {}
            
            if (is(valueAxis, "ValueAxis")) {
              .Object@drawOnAxis <- listProperties(valueAxis)
            } else if (length(valueAxis) == 1) {
              .Object@drawOnAxis <- valueAxis
            } else {
              stop("Argument 'valueAxis' non valid")
            }
            
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' stockGraphs <- list(stockGraph(comparable = TRUE), stockGraph(comparable = FALSE)) 
#' setStockGraphs(.Object =  stockPanel(), stockGraphs = stockGraphs)
#' stockPanel(stockGraphs = stockGraphs)
#' # ---
#' @rdname initialize-StockPanel
#' @export
setGeneric(name = "setStockGraphs",
           def = function(.Object, stockGraphs){ standardGeneric("setStockGraphs") })
#' @rdname initialize-StockPanel
setMethod(f = "setStockGraphs", signature = c("StockPanel", "list"),
          definition = function(.Object, stockGraphs)
          {
            rightClassElements <- prod(sapply(stockGraphs, function(element) {is(element, "AmGraph")}))
            if(!rightClassElements){
              stop("[setStockGraphs]: each element must be created with stockGraph(*)")
            }else{}
            .Object@stockGraphs <- lapply(stockGraphs, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @param stockGraph \linkS4class{AmGraph}, created with stockGraph(...).
#' Argument for method \code{addStockGraph}.
#' @examples
#' addStockGraph(.Object = stockPanel(), comparable = FALSE)
#' addStockGraph(.Object = stockPanel(), stockGraph = stockGraph(comparable = FALSE))
#' # ---
#' @rdname initialize-StockPanel
#' @export
setGeneric(name = "addStockGraph",
           def = function(.Object, stockGraph = NULL, ...){ standardGeneric("addStockGraph") })
#' @rdname initialize-StockPanel
setMethod(f = "addStockGraph", signature = c("StockPanel", "AmGraphOrMissing"),
          definition = function(.Object, stockGraph = NULL, ...)
          {
            if (is.null(stockGraph) && !missing(...)) {
              stockGraph <- stockGraph(...)
            } else if (is.null(stockGraph) && missing(...)) {
              stop("You must provide either attribute 'stockGraph' or its properties...")
            } else {}
            
            
            .Object@stockGraphs <- rlist::list.append(.Object@stockGraphs,
                                                      listProperties(stockGraph))
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' setStockLegend(.Object = stockPanel(), valueTextRegular = "[[value]]")
#' # equivalent to:
#' stockLegend_obj <- stockLegend(valueTextRegular = "[[value]]")
#' setStockLegend(.Object = stockPanel(), stockLegend = stockLegend_obj)
#' # ---
#' @rdname initialize-StockPanel
#' @export
setGeneric(name = "setStockLegend", def = function(.Object, stockLegend = NULL, ...){standardGeneric("setStockLegend") })
#' @rdname initialize-StockPanel
setMethod(f = "setStockLegend", signature = c("StockPanel", "AmLegendOrMissing"),
          definition = function(.Object, stockLegend = NULL, ...)
          {
            if (is.null(stockLegend) && !missing(...)) {
              stockLegend <- stockLegend(...)
            } else if (is.null(stockLegend) && !missing(...)) {
              stop("You must provide either argument 'stockLegend' or its properties")
            } else {}
            
            .Object@stockLegend<- listProperties(stockLegend)
            validObject(.Object)
            return(.Object)
          })