#' @include AmObject.R
NULL

#' @title PeriodSelector class
#' @author DataKnowledge
#' 
#' @slot periods
#' Object of clas \code{list}.
#' Period object has 4 properties - period, count, label and selected.
#' Possible period values are:
#' "ss" - seconds, "mm" - minutes, "hh" - hours, "DD" - days, "MM" - months and "YYYY" - years.
#' property "count" specifies how many periods this button will select.
#' "label" will be displayed on a button and "selected" is a boolean
#' which specifies if this button is selected when chart is initialized or not.
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
setClass(Class = "PeriodSelector", contains = "AmObject",
         representation = representation( periods = "list")
)

#' @title Initialize a PeriodSelector
#' @examples
#' new( "PeriodSelector" )
#' @export
setMethod(f = "initialize", signature = c("PeriodSelector"),
          definition = function(.Object, periods, ...)
          {            
            if (!missing(periods)) {
              .Object@periods <- periods
            } else {}
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####
#' @title Constructor for a PeriodSelector
#' @param periods
#' Object of clas \code{list}.
#' Period object has 4 properties - period, count, label and selected.
#' Possible period values are:
#' "ss" - seconds, "mm" - minutes, "hh" - hours, "DD" - days, "MM" - months and "YYYY" - years.
#' property "count" specifies how many periods this button will select.
#' "label" will be displayed on a button and "selected" is a boolean
#' which specifies if this button is selected when chart is initialized or not.
#' @param ...
#' Properties of PeriodSelector.
#' See \url{http://docs.amcharts.com/3/javascriptcharts/PeriodSelector}
#' @return An \code{\linkS4class{PeriodSelector}} object
#' @examples
#' periodSelector(fillAlpha = .4, value = 1)
#' periodSelector(fillAlpha = .4, adjustBorderColor = TRUE, gridThickness = 1)
#' @export
periodSelector <- function(periods, ...){
  .Object <- new(Class="PeriodSelector")
  if (!missing(periods)) {
    .Object@period <- periods
  } else {}
  .Object <-  setProperties(.Object, ...)
  return( .Object )
}

#' @exportMethod addPeriod
setGeneric("addPeriod", def = function(.Object, ...) { standardGeneric("addPeriod")} )
#' @title Add a period
#' @param .Object
#' Object of class \code{\linkS4class{PeriodSelector}}.
#' @param ...
#' Properties of period.
#' @return The updated object of class \code{\linkS4class{PeriodSelector}}.
#' @examples
#' library(pipeR)
#' periodSelector() %>>% addPeriod(period = "MM", selected = TRUE, count = 1, label= "1 month")
#' @family PeriodSelector setters
#' @family PeriodSelector methods
#' @seealso \code{\linkS4class{PeriodSelector}} S4 class
#' @rdname addPeriod
#' @export
setMethod( f = "addPeriod", signature = c("PeriodSelector"),
           definition = function(.Object, ...)
           {
             .Object@periods <- rlist::list.append( .Object@periods, list(...) )
             validObject(.Object)
             return(.Object)
           })

#' @title List properties
#' @return Properties of the object in a list
#' @importFrom rlist list.append
setMethod( f = "listProperties", signature = "PeriodSelector",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length(.Object@periods) > 0 ){
               ls <- rlist::list.append(ls, periods = .Object@periods)
             }
             return(ls)
           }
)
