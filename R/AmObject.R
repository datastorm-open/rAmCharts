#' @title AmObject class
#' @author DataKnowledge
#' @description This is a virtual class for representing any Am** class
#' 
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string. See examples for details.
#' 
#' @slot otherProperties \code{list},
#' containing other avalaible properties non coded in the package yet.
#' 
#' @slot value \code{numeric}.
#' 
#' @export
#' @family rAmChart classes
setClass(
  Class = "AmObject",
  representation = representation(value = "numeric", listeners = "list", otherProperties = "list", "VIRTUAL") 
)

#' @title Visualize with show
#' @param object \linkS4class{AmObject}
#' @examples
#' show(amSerialChart())
#' @family Visualizations
#' @export
setMethod(f = "show", signature = "AmObject",
          definition = function(object)
          {
            cat("~", class(object),"~\n")
            print(listProperties(object))
          })

#' @title Visualize with print
#' @param x \linkS4class{AmChart}
#' @param ... Other properties.
#' @examples
#' print(new("AmChart", categoryField = "variables"))
#' @family Visualizations
#' @export
setMethod(f = "print", signature = "AmObject",
          definition = function(x, ...) {print(listProperties(x))})

# > @listeners: setters ####

#' @title Methods of AmObject
#' @param .Object \code{\linkS4class{AmObject}}.
#' @param name \code{character} containing the name of the listener.
#' @param expression \code{character} containing the associated function event.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' addListener(.Object = amPieChart(),
#'             name = "clickSlice" ,
#'             expression = "function(event){ alert('ok !'); }")
#'             
#' addListener(.Object = amLegend(),
#'             name = "select",
#'             expression = paste0("function onSelect (properties) {",
#'                                 "alert('selected nodes: ' + properties.nodes);",
#'                                 "}"))
#' @rdname methods-AmObject
#' @export
setGeneric(name = "addListener", def = function(.Object, name, expression) { standardGeneric("addListener")})
#' @rdname methods-AmObject
setMethod(f = "addListener", signature = c("AmObject", "character", "character"),
          definition = function(.Object, name, expression)
          {
            .Object@listeners[[ eval(name) ]] <- htmlwidgets::JS(expression)
            validObject(.Object)
            return(.Object)
          })

# > @otherProperties: setProperties ####

#' @details If the property is a class property, it will be overwritten if the attribute is non NULL
#' @param list (Optional) \code{list} containing properties to set.
#' The former properties will be overwritten.
#' @param ... Other properties
#' @examples
#' library(pipeR)
#' # For an AmChart
#' ls <- list(categoryAxis = list(gridPosition = "start"), test = 1)
#' amPieChart() %>>% setProperties(list = ls) %>>% setProperties(fontSize = 15)
#' @rdname methods-AmObject
#' @export
setGeneric(name = "setProperties", def = function(.Object, list, ...){standardGeneric("setProperties")})
#' @rdname methods-AmObject
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

# > listProperties ####

#' @title List attributes of an S4 object
#' @param .Object any class object of the package
#' @examples
#' amChart(type = "serial")
#' @rdname listProperties-AmObject
#' @export
setGeneric(name = "listProperties", def = function(.Object){standardGeneric("listProperties")})
#' @rdname listProperties-AmObject
setMethod(f = "listProperties", signature = "AmObject",
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