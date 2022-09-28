#' View full API of amcharts.js
#'
#' View full API of amcharts.js
#' 
#' @param viewer : Set to NULL to open in a browser
#' 
#' @examples
#'
#' \dontrun{
#' amChartsAPI()
#' amChartsAPI(NULL)
#' }
#' 
#'
#' @export
#' @importFrom  utils browseURL
#' @references See online documentation \url{https://datastorm-open.github.io/introduction_ramcharts/}
#'
amChartsAPI <- function(viewer = getOption("viewer")){
  if (!is.null(viewer)){
    tempDir <- tempdir()
    ctrl <- file.copy(from = system.file("api", package = "rAmCharts"), 
                      to = tempDir, overwrite = TRUE , recursive = TRUE)
    viewer(paste0(tempDir, "/api/javascriptcharts/index.html"))
  }else{
    browseURL(system.file("api/javascriptcharts/index.html", package = "rAmCharts"))
  }
}
