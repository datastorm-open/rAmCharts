#' @include class_AmObject.R
NULL

#' @title AxisBase class
#' @author datastorm-open
#' @description Base class for ValueAxis and CategoryAxis. It can not be explicitly instantiated.
#' 
#' @slot guides \code{list}.
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must be a character string.
#' @slot otherProperties \code{list}
#' containing other avalaible properties not yet implemented in the package.
#' @slot value \code{numeric}.
#' Guides of this axis. Use addGuide method.
#' 
#' @export
#' 
setClass(Class = "AxisBase", contains = "AmObject",
         representation = representation(guides = "list", "VIRTUAL"))

