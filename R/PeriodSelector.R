#' @include AmObject.R
NULL

#' @title PeriodSelector class
#' @author DataKnowledge
#' @section Slots:
#' @slot \code{periods}: Object of clas \code{numeric}.
#' Array of predefined period objects.
#' Period object has 4 properties - period, count, label and selected.
#' Possible period values are:
#' "ss" - seconds, "mm" - minutes, "hh" - hours, "DD" - days, "MM" - months and "YYYY" - years.
#' property "count" specifies how many periods this button will select.
#' "label" will be displayed on a button and "selected" is a boolean
#' which specifies if this button is selected when chart is initialized or not.
#' Example: {period:"DD", count:10, label:"10 days", selected:false}.
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
            if(!missing(periods)){
              .Object@periods <- periods
            }
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          }
)

# CONSTRUCTOR ####
#' @title Constructor.
#' @title Constructor for a PeriodSelector
#' @param \code{...}: {Properties of PeriodSelector.
#' See \code{\url{http://docs.amcharts.com/3/javascriptcharts/PeriodSelector}}}
#' @return An \code{\linkS4class{PeriodSelector}} object
#' @examples
#' periodSelector(fillAlpha = .4, value = 1)
#' periodSelector(fillAlpha = .4, adjustBorderColor = TRUE, gridThickness = 1)
#' @export
periodSelector <- function(periods, ...){
  .Object <- new(Class="PeriodSelector")
  if(!missing(periods)){
    .Object@period <- periods
  }
  .Object <- .Object %>>% setProperties(...)
  return( .Object )
}

#' @exportMethod addPeriod
setGeneric("addPeriod", def = function(.Object, ...) { standardGeneric("addPeriod")} )
#' @title Add a period
#' @param \code{.Object}: Object of class \code{\linkS4class{PeriodSelector}}.
#' @param \code{...}: Properties of period.
#' @return The updated object of class \code{\linkS4class{PeriodSelector}}.
#' @examples
#' # Setter for period
#' periodSelector() %>>% addPeriod(  period = "MM", selected = TRUE, count = 1, label= "1 month" )
#' @family PeriodSelector setters
#' @family PeriodSelector methods
#' @seealso \code{\linkS4class{PeriodSelector}} S4 class
#' @name addPeriod
#' @rdname addPeriod
#' @export
setMethod( f = "addPeriod", signature = c("PeriodSelector"),
           definition = function(.Object, ...)
           {
             .Object@periods <- rlist::list.append( .Object@periods, list(...) )
             validObject(.Object)
             return(.Object)
           }
)

#' @title List properties
#' @return Properties of the object in a list
setMethod( f = "listProperties", signature = "PeriodSelector",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length(.Object@periods) > 0 ){
               ls <- list.append(ls, periods = .Object@periods)
             }
             return(ls)
           }
)
