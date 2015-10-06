#' @rdname amChartsOutput
#' @export
setGeneric( name = "amChartsOutput",
            def = function(outputId, type, width, height) {standardGeneric("amChartsOutput")})
#' @title SHINY
#' @description Widget output function for use in Shiny
#' @param outputId \code{character}.
#' @param type \code{character}
#' indicating the chart type.
#' @param width \code{character}.
#' @param height \code{character}.
#' @rdname amChartsOutput
setMethod(f = "amChartsOutput", signature = c("character", "character"),
  definition = function(outputId, type, width, height)
  {
    if (missing(width)) {
      width <- "100%"
    } else {}
    if (missing(height)) {
      height <- "400px"
    } else {}
    
    jsFile <- switch(type,
                     "funnel" = "amFunnelChart",
                     "gantt" = "amGanttChart",
                     "gauge" = "amAngularGauge",
                     "pie" = "amPieChart",
                     "radar" = "amRadarChart",
                     "serial" = "amSerialChart",
                     "stock" = "amStockChart",
                     "xy" = "amXYChart",
                     "drill" = "amDrillChart",
                     stop("type error")
    )
    
    htmlwidgets::shinyWidgetOutput(
      outputId = outputId,     
      name = eval(jsFile),   
      width = width,
      height = height,
      package = 'rAmCharts'
    )
  }
)

#' @title SHINY
#' @description Widget output function for use in Shiny
#' @param expr \code{expression}.
#' @param env see htmlwidgets doc.
#' @param ... other arguments.
#' @rdname renderAmCharts
#' @export
setGeneric(name = "renderAmCharts", def = function(expr, env, ...) {standardGeneric("renderAmCharts")})
#' @param quoted \code{logical}.
#' @rdname renderAmCharts
setMethod(f = "renderAmCharts", signature = "ANY",
          definition = function(expr, env, quoted){
            if (missing(env)) {
              env <- parent.frame()
            } else {}
            if (missing(quoted)) {
              quoted <- FALSE
            } else {}
            if (!quoted) {
              expr <- substitute(expr) # force quoted
            } else {}
            htmlwidgets::shinyRenderWidget(expr, amChartsOutput, env, quoted = TRUE)
          })
#' @rdname renderAmCharts
setMethod(f = "renderAmCharts", signature = "AmChart",
          definition = function(expr, env)
            {
            # message("right method")
            if (missing(env)) {
              env <- parent.frame()
            } else {}
            htmlwidgets::shinyRenderWidget(plot(expr), amChartsOutput, env, quoted = TRUE)
          })
