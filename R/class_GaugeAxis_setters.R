#' @include classUnions.R
NULL

#' @rdname GaugeAxis
#' @export
#' 
setGeneric(name = "setBands", def = function(.Object, bands){standardGeneric("setBands")})
#' @rdname GaugeAxis
#' @examples
#' # -- update 'bands' at once
#' bands <- list(gaugeBand(startValue = 70, endValue = 90),
#'               gaugeBand(startValue = 40, endValue = 60))
#' gaugeAxis(bands = bands)
#' 
setMethod(f = "setBands", signature = c("GaugeAxis", "list"),
          definition = function(.Object, bands)
          {
            rightClassElements <- prod(sapply(bands, function(element) {is(element, "GaugeBand")}))
            if (!rightClassElements) {
              stop("[setBands]: each elements of bands must be a GaugeBand")
            } else {}
            .Object@bands <- lapply(bands, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @rdname GaugeAxis
#' 
#' @param band \linkS4class{GaugeBand}.
#' Argument for method \code{addBand}.
#' 
#' @export
#' 
setGeneric(name = "addBand", def = function(.Object, band = NULL, ...) {standardGeneric("addBand")} )
#' @rdname GaugeAxis
#' @examples
#' # --- add 'band' one by one one
#' addBand(.Object = gaugeAxis(), startValue = 0, endValue = 100)
#' # equivalent to
#' gaugeBand_obj <- gaugeBand(startValue = 0, endValue = 100)
#' addBand(.Object = gaugeAxis(), band = gaugeBand_obj)
#' 
setMethod(f = "addBand", signature = c("GaugeAxis", "GaugeBandOrMissing"),
          definition = function(.Object, band = NULL, ...)
          {
            if (is.null(band) && !missing(...)) {
              band <- gaugeBand(...)
            } else if (is.null(band) && missing(...)) {
              stop("You must provide either argument 'band' or its properties")
            } else {}
            
            .Object@bands <- c(.Object@bands, list(listProperties(band)))
            return(.Object)
          })