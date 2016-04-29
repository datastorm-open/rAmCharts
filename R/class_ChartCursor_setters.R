#' @include classUnions.R
NULL

#' @examples
#' setOneBalloonOnly(.Object = chartCursor(), oneBalloonOnly = TRUE)
#' @rdname initialize-ChartCursor
#' @export
setGeneric(name = "setOneBalloonOnly", def = function(.Object, oneBalloonOnly) {standardGeneric("setOneBalloonOnly")})
#' @rdname initialize-ChartCursor
setMethod(f = "setOneBalloonOnly", signature = c("ChartCursor", "logical"),
          definition = function(.Object, oneBalloonOnly)
          {
            .Object@oneBalloonOnly <- oneBalloonOnly
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' setValueLineAxis(.Object = chartCursor(), id = "valueAxis1",
#'                  title = "Hello !", axisTitleOffset = 12)
#' # equivalent to:
#' valueLineAxis_obj <- valueAxis(id = "valueAxis1", title = "Hello !", axisTitleOffset = 12)
#' setValueLineAxis(.Object = chartCursor(), valueLineAxis  = valueLineAxis_obj)
#' # or iff 'valueLineAxis_obj' has already been added to the chart:
#' setValueLineAxis(.Object = chartCursor(), valueLineAxis  = "valueAxis1")
#' @rdname initialize-ChartCursor
#' @export
setGeneric(name = "setValueLineAxis", def = function(.Object, valueLineAxis = NULL, ...) {standardGeneric("setValueLineAxis")})
#' @rdname initialize-ChartCursor
setMethod(f = "setValueLineAxis", signature = c("ChartCursor", "ValueAxisOrCharacterOrMissing"),
          definition = function(.Object, valueLineAxis = NULL, ...)
          {
            if (is.null(valueLineAxis) && !missing(...)) {
              valueLineAxis <- valueAxis(...)
            } else if (is.null(valueLineAxis) && missing(...)) {
              stop("You must either give argument 'valueLineAxis' or its properties")
            } else {}
            
            if (is(valueLineAxis, "ValueAxis")) {
              .Object@valueLineAxis <- listProperties(valueLineAxis)
            } else if (length(valueLineAxis) == 1) {
              .Object@valueLineAxis <- valueLineAxis
            } else {
              stop("Argument 'valueLineAxis' non valid")
            }
            
            validObject(.Object)
            return(.Object)
          })
