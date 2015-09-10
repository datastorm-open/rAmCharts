#' @title CategoryAxis class
#' @author DataKnowledge

#' @slot gridPosition
#'  Object of class \code{character}.
#' Specifies if a grid line is placed on the center of a cell or on the beginning of a cell.
#' Possible values are: "start" and "middle"
#' This setting doesn't work if parseDates is set to true.
#' 
#' @slot listeners
#' Object of class \code{"list"} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' See examples for details.
#' 
#' @slot otherProperties
#' Object of class \code{"list"},
#' containing other avalaible properties non coded in the package yet.
#' 
#' @slot value
#' Object of class \code{numeric}.
#' 
#' @export
setClass( Class = "CategoryAxis", contains = "AxisBase",
  representation = representation(gridPosition = "character"))

#' @title Initialize a CategoryAxis
#' @param .Object \linkS4class{CategoryAxis}.
#' @param gridPosition \code{character}.
#' Specifies if a grid line is placed on the center of a cell or on the beginning of a cell.
#' Possible values are: "start" and "middle"
#' This setting doesn't work if parseDates is set to true.
#' @param guides \code{list} of \linkS4class{Guide}.
#' @param ... Other properties.
#' @examples
#' \donttest{
#' new("CategoryAxis")
#' new("CategoryAxis", gridPosition = "start", 1) # 1 is not take into account
#' }
#' guides <- list(guide(fillAlpha = .4, adjustBorderColor = TRUE),
#'                guide(fillAlpha = .4, adjustBorderColor = TRUE))
#' new("CategoryAxis", gridPosition = "start",  gridThickness = 1, guides = guides)
#' @rdname initialize-CategoryAxis
#' @export
setMethod(f = "initialize", signature = c("CategoryAxis"),
          definition = function(.Object, gridPosition, guides, ...)
          {            
            if (!missing(gridPosition)) {
              .Object@gridPosition <- gridPosition
            } else {}
            if (!missing(guides) && is.list(guides)) {
              .Object@guides <- lapply(guides, listProperties)
            } else {}
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####

#' @describeIn initialize-CategoryAxis
#' @examples
#' categoryAxis(gridPosition = "start", adjustBorderColor = TRUE)
#' @export
categoryAxis <- function(gridPosition, ...) {
  .Object <- new(Class="CategoryAxis")
  if (!missing(gridPosition)) {
    .Object@gridPosition <- gridPosition
  } else {}
  .Object <- setProperties(.Object, ...)
  return( .Object )
}

#' @rdname initialize-CategoryAxis
#' @export
setGeneric(name = "setGridPosition",
           def = function(.Object, gridPosition){ standardGeneric("setGridPosition") } )
#' @examples
#' setGridPosition(.Object = categoryAxis(), gridPosition = "start")
#' @rdname initialize-CategoryAxis
setMethod(f = "setGridPosition", signature = c("CategoryAxis", "character"),
          definition = function(.Object, gridPosition)
          {
            .Object@gridPosition <- gridPosition
            validObject(.Object)
            return(.Object)
          })

#' @rdname listProperties-AmObject
#' @examples
#' \dontshow{
#' library(pipeR)
#' categoryAxis(ignoreAxisWidth = TRUE) %>>% listProperties() %>>% class
#' categoryAxis(ignoreAxisWidth = TRUE) %>>% listProperties() %>>% length
#' categoryAxis(ignoreAxisWidth = TRUE) %>>% setGridPosition("start") %>>% listProperties()
#' }
setMethod(f = "listProperties", signature = "CategoryAxis",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length(.Object@gridPosition)) {
             ls <- rlist::list.append(ls, gridPosition = .Object@gridPosition)
             } else {}
             return(ls)
           })
