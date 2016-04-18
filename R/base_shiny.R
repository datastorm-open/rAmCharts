#' @title Run example with shiny
#' @description
#' See some examples in a shiny web application.
#' Both 'am' functions and basic functions are illustrated.
#' 
#' @examples 
#' if (interactive()) runExamples()
# 
#' @export 
#' 
runExamples <- function ()
{
  if (!requireNamespace(package = "shiny"))
    message("Package 'shiny' is required to run this function")
  
  if (!requireNamespace(package = "shinydashboard"))
    message("Package 'shinydashboard' is required to run this function")
  
  if (!requireNamespace(package = "pipeR"))
    message("Package 'pipeR' is required to run this function")
  
  if (!requireNamespace(package = "data.table"))
    message("Package 'data.table' is required to run this function")
  
  shiny::shinyAppDir(appDir = system.file("shiny", package="rAmCharts"))
}

