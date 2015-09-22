#' @include sharedGenerics.R classUnion.R
NULL

#' @examples
#' setFillAlpha(.Object = guide(), fillAlpha = 1)
#' @rdname initialize-Guide
#' @export
setGeneric(name = "setFillAlpha", def = function(.Object, fillAlpha) {standardGeneric("setFillAlpha")})
#' @rdname initialize-Guide
setMethod(
  f = "setFillAlpha",
  signature = c("Guide", "numeric"),
  definition = function(.Object, fillAlpha)
  {
    .Object@fillAlpha <- fillAlpha
    validObject(.Object)
    return(.Object)
  })

#' @examples
#' valueAxis_obj <- valueAxis(test = "foo")
#' setValueAxis(.Object = guide(), valueAxis = valueAxis_obj)
#' @rdname initialize-Guide
setMethod(f = "setValueAxis", signature = c("Guide", "ValueAxisOrCharacterOrMissing"),
          definition = function(.Object, valueAxis = NULL, ...)
          {
            if (is.null(valueAxis) && !missing(...)) {
              .Object@valueAxis <- valueAxis
            } else if (is.null(valueAxis) && missing(...)) {
              stop("You must provide either argument 'valueAxis' or its properties")
            } else {}
            
            if (is(valueAxis,"ValueAxis")) {
              .Object@valueAxis <- listProperties(valueAxis)
            } else if (length(valueAxis) == 1) {
              .Object@valueAxis <- valueAxis
            } else {
              stop("Argument 'valueAxis' non valid")
            }
            
            validObject(.Object)
            return(.Object)
          })