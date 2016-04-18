#' @include classUnion.R
NULL

#' @rdname initialize-GaugeAxis
#' @export
setGeneric(name = "setBands", def = function(.Object, bands){standardGeneric("setBands")})
#' @examples
#' bands <- list(gaugeBand(startValue = 70, endValue = 90),
#'               gaugeBand(startValue = 40, endValue = 60))
#' gaugeAxis(bands = bands)
#' # ---
#' @rdname initialize-GaugeAxis
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

#' @param band \linkS4class{GaugeBand}.
#' Argument for method \code{addBand}.
#' @rdname initialize-GaugeAxis
#' @export
setGeneric(name = "addBand", def = function(.Object, band = NULL, ...) {standardGeneric("addBand")} )
#' @examples
#' addBand(.Object = gaugeAxis(), startValue = 0, endValue = 100)
#' # equivalent to
#' gaugeBand_obj <- gaugeBand(startValue = 0, endValue = 100)
#' addBand(.Object = gaugeAxis(), band = gaugeBand_obj)
#' # ---
#' @rdname initialize-GaugeAxis
setMethod(f = "addBand", signature = c("GaugeAxis", "GaugeBandOrMissing"),
          definition = function(.Object, band = NULL, ...)
          {
            if (is.null(band) && !missing(...)) {
              band <- gaugeBand(...)
            } else if (is.null(band) && missing(...)) {
              stop("You must provide either argument 'band' or its properties")
            } else {}
            
            .Object@bands <- rlist::list.append(.Object@bands, listProperties(band))
            return(.Object)
          })