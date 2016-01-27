#' @title AmObject class
#' @author DataKnowledge
#' @description This is a virtual class for representing any Am** class
#' 
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string. See examples for details.
#' @slot otherProperties \code{list},
#' containing other avalaible properties non coded in the class.
#' @slot value \code{numeric}.
#' 
#' @export
setClass(Class = "AmObject",
         representation = representation(
           value = "numeric",
           listeners = "list",
           otherProperties = "list", "VIRTUAL"))

#' @title Visualize with show
#' @description Display the object in the console.
#' @param object \linkS4class{AmObject}
#' @examples
#' pipeR::pipeline(
#'   rAmCharts::amPieChart(valueField = "value", titleField = "key", creditsPosition = "top-right",
#'                         backgroundColor = "#7870E8"),
#'   rAmCharts::setDataProvider(data.frame(key = c("FR", "US"), value = c(20,10))),
#'   rAmCharts::setExport(position = "bottom-left")
#' )
#' @export
#' 
setMethod(f = "show", signature = "AmObject",
          definition = function(object)
          {
            cat("~", class(object),"~\n")
            print(listProperties(object))
          })

#' @title Visualize with print
#' @description Display the object in the console.
#' @param x \linkS4class{AmChart}.
#' @param withDetail \code{logical}. Should the detail be printed ?
#' @param ... Other properties.
#' @examples
#' print(new("AmChart", categoryField = "variables", type = "serial"))
#' print(new("AmChart", categoryField = "variables", type = "serial"), withDetail = FALSE)
#' @details If the object possess a 'dataProvider' property, it will be hidden in the console.
#' To see if it's correctly registered use '@dataProvider'.
#' @export
setMethod(f = "print", signature = "AmObject",
          definition = function(x, withDetail = TRUE,...) {
            if (withDetail) {
              cat("~ ", class(x)," object (printed with detail) ~\n\n")
              
              cat("Referenced properties:\n")
              ls <- listProperties(x)
              cat(paste(names(ls), collapse = ", "), "\n\n")
              
              cat("   - Detail:\n")
              if (exists(x = "dataProvider", where = ls)) {
                ls["dataProvider"] <- NULL
              } else {}
                
              print(ls)
            } else {
              cat("~ ", class(x)," object (printed without detail)~\n\n")
              cat("Referenced properties:\n")
              ls <- listProperties(x)
              cat(paste(names(ls), collapse = ", "))
            }
            cat("\n")
          })

# > @listeners: setters ####

#' @title Methods of AmObject
#' @description Methods for inherited classes.
#' 
#' @param .Object \code{\linkS4class{AmObject}}.
#' @param name \code{character} containing the name of the listener.
#' @param expression \code{character} containing the associated function event.
#' 
#' @return The updated object.
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

#' @param list_prop (Optional) \code{list} containing properties to set.
#' The former properties will be overwritten.
#' @param ... Other properties
#' 
#' @examples
#' 
#' library(pipeR)
#' # either you can set a list
#' ls <- list(categoryAxis = list(gridPosition = "start"), fontSize = 15)
#' amSerialChart() %>>% setProperties(list = ls)  %>>% print()
#' 
#' # or you can set one or more properties
#' amPieChart() %>>% setProperties(handDrawn = TRUE, fontSize = 15) %>>% print()
#' 
#' # overwrite a property
#' amPieChart() %>>%  setProperties(fontSize = 15) %>>%  setProperties(fontSize = 12) %>>% print()
#' 
#' # warning if you try to set a property which is a slot...
#' # in that case, use the setter methods 'setXX' or 'addXX' which check the validity
#' \dontrun{
#' amPieChart() %>>% setProperties(type = "serial") %>>% print()
#' }
#' 
#' amPieChart() %>>% setExport()
#' 
#' @details Former properties will be overwritten.
#' 
#' @rdname methods-AmObject
#' @export
#' 
setGeneric(name = "setProperties", def = function(.Object, list_prop, ...){standardGeneric("setProperties")})
#' @rdname methods-AmObject
setMethod(f = "setProperties", signature = c(.Object = "AmObject"),
          definition = function(.Object, list_prop, ...)
          {
            if (missing(list_prop)) {
              newProperties <- list(...)
              # Different cases have to be considered since the properties
              # may be referenced as slots, in that case a warning is sent.
              lapply(X = names(newProperties), FUN = function(prop) {
                if (prop %in% slotNames(.Object)) {
                  # if it's a slot, a warning is sent
                  warning("You should use setter for property '", prop, "'")
                  slot(.Object, prop, check = TRUE) <<- newProperties[[prop]]
                } else {
                  .Object@otherProperties[[prop]] <<- newProperties[[prop]]
                }
                invisible()
              })
            } else if (is.list(list_prop)) {
              .Object@otherProperties <- list_prop
            } else {}
            
            validObject(.Object)
            return(.Object)
          })

# > listProperties ####

#' @title List properties of an S4 object
#' @description Each S4 class implement the method to list its properties (usefull for update complex properties).
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