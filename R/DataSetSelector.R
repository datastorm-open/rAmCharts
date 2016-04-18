#' @include AmObject.R
NULL

#' @title DataSetSelector class
#' @author DataKnowledge
#' 
#' @description DataSetSelector is a tool for selecting data set's
#' as main and for comparing with main data set.
#' @details Run \code{api("DataSetSelector")} for more information and all avalaible properties.
#' @slot position \code{character}.
#' Possible values: "right", "left", "top", "bottom".
#' "top" and "bottom" positions has a limitation -
#' only one data set can be selected for comparing.
#' 
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' See examples for details.
#' @slot otherProperties \code{list},
#' containing other avalaible properties non coded in the package yet.
#' @slot value \code{numeric}.
#' 
#' @export
setClass(Class = "DataSetSelector", contains = "AmObject",
         representation = representation(position = "character"))

#' @title Initialize  DataSetSelector
#' @param .Object \linkS4class{DataSetSelector}
#' @param position \code{character}.
#' Possible values: "right", "left", "top", "bottom".
#' "top" and "bottom" positions has a limitation -
#' only one data set can be selected for comparing.
#' @param ... Other properties.
#' @return (updated) \linkS4class{DataSetSelector}
#' @examples
#' new("DataSetSelector", size = 10)
#' @rdname initialize-DataSetSelector
#' @export
setMethod(f = "initialize", signature = "DataSetSelector",
          definition = function(.Object, position, ...)
          {  
            if (!missing(position)) {
              .Object <- setPosition(.Object, position)
            } else {}
            
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####
#' @describeIn initialize-DataSetSelector
#' @examples
#' dataSetSelector(position = "left")
#' @export
dataSetSelector <- function(position, ...) {
  .Object <- new("DataSetSelector")
  
  if (!missing(position)) {
    .Object <- setPosition(.Object, position)
  } else {}
  
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return(.Object)
}

# > @position : setters ####

#' @examples
#' setPosition(.Object = dataSetSelector(), position = "left")
#' @rdname initialize-DataSetSelector
#' @export
setGeneric(name = "setPosition",
           def = function(.Object, position) {standardGeneric("setPosition")} )
#' @examples
#' setPosition(.Object = dataSetSelector(), position = "left")
#' @rdname initialize-DataSetSelector
setMethod(f = "setPosition", signature = c("DataSetSelector", "character"),
          definition = function(.Object, position)
          {
            .Object@position <- position
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' dataSetSelector(position = "left")
#' @rdname listProperties-AmObject
setMethod(f = "listProperties", signature = "DataSetSelector",
          definition = function(.Object)
          { 
            ls <- callNextMethod()
            if (length(.Object@position)) {
              ls <- rlist::list.append(ls, position = .Object@position)
            } else{}
            return(ls)
          })