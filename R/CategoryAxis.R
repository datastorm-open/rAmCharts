#' @title CategoryAxis class
#' @author DataKnowledge
#' @section Slots:
#' @slot \code{gridPosition}: { Object of class \code{character}.
#' Specifies if a grid line is placed on the center of a cell or on the beginning of a cell.
#' Possible values are: "start" and "middle" This setting doesn't work if parseDates is set to true.}
#' @export
setClass( Class = "CategoryAxis", contains = "AxisBase",
  representation = representation( gridPosition = "character" )
)

#' @title Initialize a CategoryAxis
#' @param \code{gridPosition}: {Object of class \code{character}.}
#' @param \code{guides}: {Object of class \code{list}. List of guides.}
#' @examples
#' \donttest{
#' new("CategoryAxis")
#' new("CategoryAxis", gridPosition = "start", 1) # 1 is not take into account
#' }
#' guides <- list(guide(fillAlpha = .4, adjustBorderColor = TRUE), guide(fillAlpha = .4, adjustBorderColor = TRUE))
#' new("CategoryAxis", gridPosition = "start",  gridThickness = 1, guides = guides)
#' @export
setMethod(f = "initialize", signature = c("CategoryAxis"),
          definition = function(.Object, gridPosition, guides, ...)
          {            
            if(!missing(gridPosition)){
              .Object@gridPosition <- gridPosition
            }
            if(!missing(guides) && is.list(guides)){
              .Object@guides <- lapply(guides, listProperties)
            }else{}
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          }
)

# CONSTRUCTOR ####
#' @title
#' #’ Constructor.
#' @title Constructor for an AmGraph
#' @param \code{...}: {Properties of CategoryAxis.
#' See \code{\url{http://docs.amcharts.com/3/javascriptcharts/CategoryAxis}}}
#' @return An \code{\linkS4class{CategoryAxis}} object
#' @examples
#' new("CategoryAxis", gridPosition = "start")
#' categoryAxis(gridPosition = "start", adjustBorderColor = TRUE)
#' @export
categoryAxis <- function(gridPosition, ...){
  .Object <- new(Class="CategoryAxis")
  if(!missing(gridPosition)){
    .Object@gridPosition <- gridPosition
  }
  .Object <- setProperties(.Object, ...)
  return( .Object )
}

#' @exportMethod setGridPosition
setGeneric(name = "setGridPosition",
           def = function(.Object, gridPosition){ standardGeneric("setGridPosition") } )
#' @title SETTER
#' @examples
#' library(pipeR)
#' categoryAxis() %>>% setGridPosition("start")
#' @rdname setGridPosition
#' @export
setMethod(
  f = "setGridPosition",
  signature = c("CategoryAxis", "character"),
  definition = function(.Object, gridPosition)
  {
    .Object@gridPosition <- gridPosition
    validObject(.Object)
    return(.Object)
  }
)


#' @title List properties
#' @return Properties of the object in a list
#' @examples
#' \dontshow{
#' library(pipeR)
#' categoryAxis(ignoreAxisWidth = TRUE) %>>% listProperties() %>>% class
#' categoryAxis(ignoreAxisWidth = TRUE) %>>% listProperties() %>>% length
#' categoryAxis(ignoreAxisWidth = TRUE) %>>% setGridPosition("start") %>>% listProperties()
#' }
#' @importFrom rlist list.append
setMethod( f = "listProperties", signature = "CategoryAxis",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if( length(.Object@gridPosition) > 0 ){
             ls <- rlist::list.append(ls, gridPosition = .Object@gridPosition)
             }
             if( length( .Object@guides ) > 0 ){
               ls <- c(ls, guides)
             }
             return(ls)
           }
)
