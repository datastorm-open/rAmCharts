#' @include sharedGenerics.R classUnion.R
NULL

#' @examples
#' setInitialValue(.Object = trendLine(), initialValue = 16)
#' @rdname TrendLine
#' @export
setGeneric(name = "setInitialValue",
           def = function(.Object, initialValue) { standardGeneric("setInitialValue") })
#' @rdname TrendLine
setMethod(f = "setInitialValue", signature = c("TrendLine", "numeric"),
          definition = function(.Object, initialValue)
          {
            .Object@initialValue <- initialValue
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' setInitialXValue(.Object = trendLine(), initialXValue = 16)
#' @rdname TrendLine
#' @export
setGeneric(name = "setInitialXValue",
           def = function(.Object, initialXValue) { standardGeneric("setInitialXValue") })
#' @rdname TrendLine
setMethod(f = "setInitialXValue", signature = c("TrendLine", "numeric"),
          definition = function(.Object, initialXValue)
          {
            .Object@initialXValue <- initialXValue
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' setFinalValue(.Object = trendLine(), finalValue = 16)
#' @rdname TrendLine
#' @export
setGeneric(name = "setFinalValue",
           def = function(.Object, finalValue) { standardGeneric("setFinalValue") })
#' @rdname TrendLine
setMethod(f = "setFinalValue", signature = c("TrendLine", "numeric"),
          definition = function(.Object, finalValue)
          {
            .Object@finalValue <- finalValue
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' setFinalXValue(.Object = trendLine(), finalXValue = 16)
#' @rdname TrendLine
#' @export
setGeneric(name = "setFinalXValue",
           def = function(.Object, finalXValue) { standardGeneric("setFinalXValue") })
#' @rdname TrendLine
setMethod(f = "setFinalXValue", signature = c("TrendLine", "numeric"),
          definition = function(.Object, finalXValue)
          {
            .Object@finalXValue <- finalXValue
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' setValueAxis(.Object = trendLine(), id = "valueAxis-1",
#'              title = "Hello !", axisTitleOffset = 12)
#' # equival to:
#' valueAxis_obj <- valueAxis(id = "valueAxis-1", title = "Hello !", axisTitleOffset = 12)
#' trendLine(valueAxis = valueAxis_obj)
#' # or...
#' trendLine(valueAxis = "valueAxis-1")
#' # valid if and only if 'valueAxis_obj' has already been added to the chart
#' 
#' @rdname TrendLine
#' 
setMethod(f = "setValueAxis", signature = c("TrendLine", "ValueAxisOrCharacterOrMissing"),
          definition = function(.Object, valueAxis = NULL, ...)
          {
            if (is.null(valueAxis) && !missing(...)) {
              valueAxis <- valueAxis(...)
            } else if (is.null(valueAxis) && missing(...)) {
              stop("You must give either argument 'valueAxis' or its properties")
            } else {}
            
            if (is(valueAxis, "ValueAxis")) {
              .Object@valueAxis <- listProperties(valueAxis)
            } else if (length(valueAxis) == 1) {
              .Object@valueAxis <- valueAxis
            } else {
              stop("Argument 'valueAxis' non valid")
            }
            
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' setValueAxisX(.Object = trendLine(), id = "valueAxisX-1",
#'               title = "Hello !", axisTitleOffset = 12)
#' # equival to:
#' valueAxisX_obj <- valueAxis(id = "valueAxisX-1", title = "Hello !", axisTitleOffset = 12)
#' trendLine(valueAxisX = valueAxisX_obj)
#' # or...
#' trendLine(valueAxisX = "valueAxisX-1")
#' # valid if and only if 'valueAxisX_obj' has already been added to the chart
#' 
#' @rdname TrendLine
#' @export
setGeneric(name = "setValueAxisX",
           def = function(.Object, valueAxisX = NULL, ...) {standardGeneric("setValueAxisX")})
#' @rdname TrendLine
setMethod(f = "setValueAxisX", signature = c("TrendLine", "ValueAxisOrCharacterOrMissing"),
          definition = function(.Object, valueAxisX = NULL, ...)
          {
            if (is.null(valueAxisX) && !missing(...)) {
              valueAxisX <- valueAxis(...)
            } else if (is.null(valueAxisX) && missing(...)) {
              stop("You must give either argument 'valueAxisX' or its properties")
            } else {}
            
            if (is(valueAxisX, "ValueAxis")) {
              .Object@valueAxisX <- listProperties(valueAxisX)
            } else if (length(valueAxisX) == 1) {
              .Object@valueAxisX  <- valueAxisX
            } else {
              stop("Argument 'valueAxisX' non valid")
            }
            
            validObject(.Object)
            return(.Object)
          })