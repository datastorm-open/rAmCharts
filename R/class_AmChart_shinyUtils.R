#' @title SHINY
#' 
#' @description Widget output function for use in Shiny
#' @param outputId \code{character}, output variable to read the chart from.
#' @param type \code{character}, indicating the chart type.
#' @param width \code{character}, the width of the chart container.
#' @param height \code{character}, the height of the chart container.
#' 
#' @export
#' @rdname amChartsOutput
#' 
amChartsOutput <- function(outputId, type = NULL, width = "100%", height = "400px")
{
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

#' @title Tests the class of an exepression.
#' @description Only used in 'renderAmCharts'.
#' @param x expression passed to 'renderAmCharts'.
#' Either an expression that generates an HTML widget, either an
#' expression that generates an AmChart.
#' @details This function has only an internal purpose. Never use it.
#' @export
controlShinyPlot <- function(x) {
  if (!"htmlwidget" %in% class(x) &&
      (is(x, "AmChart") || is(x, "AmStockChart"))) {
    plot(x)
  } else {
    x
  }
}

#' @title SHINY
#' @description Widget output function to use in Shiny.
#' @param expr an expression that generates an HTML widget.
#' @param env the environment in which expr must be evaluated.
#' @param quoted is expr a quoted expression (with quote()).
#' This is useful if you want to save an expression into variable.
#' @name renderAmCharts
#' @rdname renderAmCharts
#' @export
renderAmCharts <- function(expr, env = parent.frame(), quoted = FALSE)
{
  if (!quoted) {
    expr <- substitute(controlShinyPlot(expr))
  } # force quoted
  
  htmlwidgets::shinyRenderWidget(expr, amChartsOutput, env, quoted = TRUE)
}