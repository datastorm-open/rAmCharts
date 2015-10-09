#' @rdname amChartsOutput
#' @export
setGeneric( name = "amChartsOutput",
            def = function(outputId, type = NULL, width, height) {standardGeneric("amChartsOutput")})
#' @title SHINY
#' @description Widget output function for use in Shiny
#' @param outputId \code{character}.
#' @param type \code{character}
#' indicating the chart type.
#' @param width \code{character}.
#' @param height \code{character}.
#' @rdname amChartsOutput
setMethod(f = "amChartsOutput", signature = c("character", "characterOrMissing"),
  definition = function(outputId, type = NULL, width, height)
  {
    if (missing(width)) {
      width <- "100%"
    } else {}
    if (missing(height)) {
      height <- "400px"
    } else {}
    
    jsFile <- ifelse(test = !is.null(type) && type == "drill",
                     yes = "amDrillChart",
                     no = "ramcharts_base") 
    
    htmlwidgets::shinyWidgetOutput(
      outputId = outputId,     
      name = eval(jsFile),   
      width = width,
      height = height,
      package = 'rAmCharts'
    )
  }
)

#' @title Test the class of an exepression
#' @description Only used in the package in 'renderAmCharts'.
#' @param x expression passed to 'renderAmCharts'.
#' Either an expression that generates an HTML widget.
#' Or an expression that generates an AmChart.
#' @noRd
controlShinyPlot <- function(x) {
  if (!"htmlwidget" %in% class(x) && (is(x, "AmChart") || is(x, "AmStockChart"))) {
    plot(x)
  } else {
    x
  }
}

#' @title SHINY
#' @description Widget output function for use in Shiny
#' @param expr an expression that generates an HTML widget.
#' @param env the environment in which to evaluate expr.
#' @param quoted is expr a quoted expression (with quote())?
#' This is useful if you want to save an expression in a variable.
#' @name renderAmCharts
#' @rdname renderAmCharts
#' @export
renderAmCharts <- function(expr, env, quoted){
  if (missing(env)) {
    env <- parent.frame()
  } else {}
  if (missing(quoted)) {
    quoted <- FALSE
  }
  if (!quoted) { expr <- substitute(rAmCharts:::controlShinyPlot(expr)) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, amChartsOutput, env, quoted = TRUE)
}