#' @include AxisBase.R sharedGenerics.R
NULL

#' @title ValueAxis class
#' @author DataKnowledge
#' @section Slots:
#' @slot \code{title}: { Object of class \code{character}. Title of the axis..}
#' @export
setClass( Class = "ValueAxis", contains = "AxisBase",
  representation = representation( title = "character" )
)

#' @title Initialize
#' @param \code{title}: {Object of class \code{character}}
#' @param \code{guides}: {Object of class \code{list}. List of guides.}
#' @examples
#' \dontshow{
#' new("ValueAxis", title = "Hello !", 1) # 1 is not take into account
#' listProperties(guide(fillAlpha = .4))
#' lapply(list(guide(fillAlpha = .4), guide(fillAlpha = .5)), listProperties)
#' }
#' 
#' \donttest{
#' # If one element of guides is not a Guide object, it shows an error
#' guides <- list(guide(fillAlpha = .4), b = 1)
#' new("ValueAxis", title = "Hello !",  gridThickness = 1, guides = guides)
#' }
#' 
#' guides <- list(guide(fillAlpha = .4), guide(fillAlpha = .5))
#' new("ValueAxis", title = "Hello !",  gridThickness = 1, guides = guides)
#' @export
setMethod(f = "initialize", signature = c("ValueAxis"),
          definition = function(.Object, title, guides, ...)
          {            
            if(!missing(title)){
              .Object@title <- title
            }else{}
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
#' @param \code{...}: {Properties of ValueAxis.
#' See \code{\url{http://docs.amcharts.com/3/javascriptcharts/ValueAxis}}}
#' @return An \code{\linkS4class{ValueAxis}} object
#' @examples
#' valueAxis(title = "Hello !", axisTitleOffset = 12)
#' @export
valueAxis <- function(title, ...){
  .Object <- new(Class="ValueAxis")
  if(!missing(title)){
    .Object@title <- title
  }
  .Object <- .Object %>>% setProperties(...)
  return( .Object )
}

#' @title SETTER
#' @examples
#' valueAxis() %>>% setTitle("Hello !")
#' @export
setMethod( f = "setTitle", signature = c("ValueAxis", "character"),
  definition = function(.Object, title)
  { 
    .Object@title <- title
    validObject(.Object)
    return(.Object)
  }
)

#' @title List properties
#' @return Properties of the object in a list
#' @examples
#' \dontshow{
#' valueAxis( axisTitleOffset = 12, tickLength = 10 ) %>>% listProperties %>>% class
#' }
#' valueAxis( axisTitleOffset = 12, tickLength = 10 ) %>>%
#' addGuide( fillAlpha = .4, adjustBorderColor = TRUE, gridThickness = 1 ) %>>%
#' setProperties( axisTitleOffset = 12 ) %>>% listProperties
setMethod( f = "listProperties", signature = "ValueAxis",
           definition = function( .Object )
           { 
             ls <- callNextMethod()
             if( length(.Object@title) > 0 ){
               ls <- list.append(ls, title = .Object@title)
             }
             return(ls)
           }
)
