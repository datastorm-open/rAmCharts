#' @include AmObject.R
NULL

#' @title AxisBase class
#' @author DataKnowledge
#' @description Base class for ValueAxis and CategoryAxis. It can not be instantiated explicitly.
#' @slot guides \code{list}.
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' See examples for details.
#' @slot otherProperties \code{list},
#' containing other avalaible properties non coded in the package yet.
#' @slot value \code{numeric}.
#' Guides belonging to this axis. Use addGuide method
#' @export
setClass(Class = "AxisBase", contains = "AmObject",
         representation = representation(guides = "list", "VIRTUAL"))

#' @rdname listProperties-AmObject
setMethod(f = "listProperties", signature = "AxisBase",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length(.Object@guides)) {
               ls <- rlist::list.append(ls, guides = .Object@guides)
             } else {}
             return(ls)
           })
