#' @title AmObject class
#' @author DataKnowledge
#' @description This is a virtual class for representing any Am** class
#' 
#' @slot listeners
#' Object of class \code{"list"} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string. See examples for details.
#' 
#' @slot otherProperties
#' Object of class \code{"list"},
#' containing other avalaible properties non coded in the package yet.
#' 
#' @slot value
#' Object of class \code{numeric}.
#' 
#' @export
#' @family rAmChart classes
setClass(
  Class = "AmObject",
  representation = representation(value = "numeric", listeners = "list", otherProperties = "list", "VIRTUAL") 
)

#' @title Visualize with show
#' @param object: \code{\linkS4class{AmObject}} object
#' @examples
#' new("AmChart")
#' @export
#' @family AmObject methods
#' @family Visualizations
#' @seealso \code{\linkS4class{AmObject}}
setMethod(f = "show", signature = "AmObject",
          definition = function(object) {print(listProperties(object))})

#' @title Visualize with print
#' @param x: an \code{\linkS4class{AmChart}} 
#' @examples
#' print(new("AmChart", categoryField = "variables"))
#' @export
#' @family AmObject methods
#' @family Visualizations
#' @seealso \code{\linkS4class{AmObject}}
setMethod(f = "print", signature = "AmObject",
          definition = function(x, ...) { print(listProperties(x)) }
)

# > @listeners: setters ####

#' @exportMethod addListener
setGeneric(name = "addListener", def = function(.Object, name, expression) { standardGeneric("addListener") } )

#' @title Setter for Listener
#' @param \code{.Object}: Object of of inherited class \code{\linkS4class{AmObject}}.
#' @param \code{name}: Object of class \code{character} containing the name of the listener.
#' @param \code{expression}: Object of class \code{character} containing the associated function event.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples 
#' library(pipeR)
#' amChart() %>>% addListener("select", "function onSelect (properties) {
#'      alert('selected nodes: ' + properties.nodes);}")
#' amLegend() %>>% addListener("select", "function onSelect (properties) {
#'      alert('selected nodes: ' + properties.nodes);}")
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{AmLegend}} S4 class
#' @rdname addListener
#' @export
setMethod (f = "addListener", signature = c("AmObject", "character", "character"),
           definition = function(.Object, name, expression)
           {
             .Object@listeners[[ eval(name) ]] <- JS(expression)
             # cat( class(JS(expression)), "\n")
             # cat( class( .Object@listeners[[ eval(name) ]] ), '\n' )
             validObject(.Object)
             return(.Object)
           }
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
            if (missing(list)) {
              .Object@otherProperties <- rlist::list.append(.Object@otherProperties, ...)
            } else if (is.list(list)) {
              .Object@otherProperties <- list
            } else {}
            validObject(.Object)
            return(.Object)
          })

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
#' @param .Object any class object of the package
#' @examples
#' library(pipeR)
#' object <- amChart() %>>% addListener("select", "function onSelect (properties) {
#'      alert('selected nodes: ' + properties.nodes);}")
#' @rdname listProperties-AmObject
#' @export
setGeneric(name = "listProperties", def = function(.Object){standardGeneric("listProperties")})
#' @rdname listProperties-AmObject
setMethod( f = "listProperties", signature = "AmObject",
           definition = function(.Object) {
             if (length(.Object@otherProperties)) {
               properties <- .Object@otherProperties
             } else {
               properties <- list()
             }
             if (length(.Object@listeners)) {
               properties <- rlist::list.append(properties, listeners = .Object@listeners)
             } else {}
             return(properties)
           })