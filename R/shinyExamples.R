#' @title Few shiny examples
#' @description Launch a shiny app with amChart examples
#' @examples
#' \dontrun{
#' runShinyExamples()
#' }
#' @import data.table
#' @export
runShinyExamples <- function()
{
  if (!requireNamespace("pipeR")) {
    stop("Please install pipeR for running this function")
  } else {}
  if (!requireNamespace("shiny")) {
    stop("Please install shiny for running this function")
  } else {}
  
  appDir <- system.file("shiny", package = "rAmCharts")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir = appDir)
}