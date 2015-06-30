#' @title AmObject class
#' @author DataKnowledge
#' @description This is a virtual class for representing any Am** class
#' @section Slots:
#' @slot \code{otherProperties}: Object of class \code{"list"},
#' containing other avalaible properties non coded in the package yet
#' @slot \code{otherProperties}: Object of class \code{"numeric"}
#' @export
#' @family rAmChart classes
setClass(
  Class = "AmObject",
  representation = representation(value = "numeric", otherProperties = "list", "VIRTUAL") 
)

#' @title Visualize with show
#' @param object: \code{\linkS4class{AmObject}} object
#' @examples
#' new("AmChart")
#' @export
#' @family AmObject methods
#' @family Visualizations
#' @seealso \code{\linkS4class{AmObject}}
setMethod( f = "show", signature = "AmObject",
           definition = function(object) { print(listProperties(object)) }
)

#' @title Visualize with print
#' @param object: an \code{\linkS4class{amSerialChart}} object   
#' @examples
#' print(new("AmChart", categoryField = "variables"))
#' @export
#' @family AmObject methods
#' @family Visualizations
#' @seealso \code{\linkS4class{AmObject}}
setMethod(f = "print", signature = "AmObject",
          definition = function(x, ...) { print(listProperties(x)) }
)


# > @otherProperties: setProperties ####

#' @exportMethod setProperties
setGeneric(name = "setProperties", def = function(.Object, list, ...){standardGeneric("setProperties")})
#' @title Add a property (non referenced as an attribute) to an AmObject
#' @description In case the property is a class attribute, it will be overwritten if the attribute is non NULL
#' @param \code{properties}: {(Optional) Object of class \code{list} containing properties to set.
#' The former properties will be overwritten.}
#' @examples
#' library(pipeR)
#' # For an AmChart
#' ls <- list(categoryAxis = list(gridPosition = "start"), test = 1)
#' amChart() %>>% setProperties(list = ls) %>>% setType("pie") %>>% setProperties(fontSize = 15)
#' @rdname setProperties
#' @family AmObjects methods
#' @family AmObject setters
#' @seealso \code{\linkS4class{AmObject}}
#' @importFrom rlist list.append
#' @export
setMethod(f = "setProperties", signature = c(.Object = "AmObject"),
          definition = function(.Object, list, ...)
          {
            if(missing(list)){
              .Object@otherProperties <- rlist::list.append(.Object@otherProperties, ...)
            }else if(is.list(list)){
              .Object@otherProperties <- list
            }else{}
            validObject(.Object)
            return(.Object)
          }
)

# > @otherProperties: getters ####

#' @exportMethod getOtherProperties
setGeneric( name = "getOtherProperties",
            def = function(.Object) { standardGeneric("getOtherProperties") } )
#' @title Getter
#' @examples
#' library(pipeR)
#' ls <- list(categoryAxis = list(gridPosition = "start"), test = 1)
#' amPieChart( otherProperties = ls) %>>% getOtherProperties
#' @rdname getOtherProperties
#' @family AmObjects methods
#' @family AmObject getters
#' @seealso \code{\linkS4class{AmObject}}
#' @export
setMethod( f = "getOtherProperties", definition = function(.Object) { return(.Object@otherProperties) } )

# > listProperties ####

#' @title List attributes of an S4 object
#' @exportMethod listProperties
setGeneric(name = "listProperties", def = function(.Object){standardGeneric("listProperties")})
#' @title List attributes of an AmObject
#' @details Factor the code for the commone attributes
setMethod( f = "listProperties", signature = "AmObject",
  definition = function(.Object) { return(.Object@otherProperties) }
)