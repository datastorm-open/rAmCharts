#' @include utils_sharedGenerics.R classUnions.R
NULL

#' @title Add a Guide for AxisBase
#' @description Update an object of inherited class \linkS4class{AxisBase}.
#' 
#' @param .Object children class of \linkS4class{AxisBase}.
#' @param guide (optional) \linkS4class{Guide}.
#' @param ... properties of \linkS4class{Guide}
#' Argument for method \code{addGuide}.
#' 
#' @return (possibly updated) .Object of class \linkS4class{AxisBase}.
#' 
#' @examples
#' addGuide(.Object = valueAxis(), fillAlpha = .4, adjustBorderColor = TRUE, gridThickness = 1)
#' # equivalent to:
#' guide_obj <- guide(fillAlpha = .4, adjustBorderColor = TRUE, gridThickness = 1)
#' addGuide(.Object = valueAxis(), guide = guide_obj)
#' 
#' @rdname initialize-AxisBase
setMethod(f = "addGuide", signature = c("AxisBase", "GuideOrMissing"),
          definition = function(.Object, guide = NULL, ...)
          {
            if (is.null(guide) && !missing(...)) {
              guide <- guide(...)
            } else if (is.null(guide) && missing(...)) {
              stop("You must give either argument 'guide' or its properties")
            } else {}
            
            .Object@guides <- c(.Object@guides, list(listProperties(guide)))
            validObject(.Object)
            return(.Object)
          })