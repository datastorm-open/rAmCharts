#' @include classUnion.R
NULL

#' @rdname GaugeArrow
#' @export
#' 
setGeneric(name = "setAxis", def = function(.Object, axis = NULL, ...) {standardGeneric("setAxis")})
#' @examples
#' # -- update 'axis' property
#' setAxis(.Object = gaugeArrow(), id = "axis1", startValue = 0,
#'         endValue = 100, valueInterval = 10)
#' # equivalent to:
#' axis_obj <- gaugeAxis(id = "axis1", startValue = 0, endValue = 100, valueInterval = 10)
#' setAxis(.Object = gaugeArrow(), axis = axis_obj)
#' # or, iff, 'axis_obj' has already been added to the chart
#' setAxis(.Object = gaugeArrow(), axis = "axis1")
#' 
#' @rdname GaugeArrow
setMethod(f = "setAxis", signature = c("GaugeArrow", "GaugeAxisOrCharacterOrMissing"),
          definition = function(.Object, axis = NULL, ...)
          {
            if (is.null(axis) && !missing(...)) {
              axis <- gaugeAxis(...)
            } else if (is.null(axis) && missing(...)) {
              stop("You must either give argument 'axis' or its properties")
            } else {}
            
            if (is(axis, "GaugeAxis")) {
              .Object@axis <- listProperties(axis)
            } else if (length(axis) == 1) {
              .Object@axis <- axis
            } else {
              stop("Argument 'axis' non valid")
            }
            
            validObject(.Object)
            return(.Object)
          })