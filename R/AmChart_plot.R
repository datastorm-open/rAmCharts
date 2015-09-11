#' @include AmChart.R
NULL

#' @title PLOTTING METHOD
#' @description Basic method to plot an AmChart 
#' @details Plots an object of class \code{\linkS4class{AmChart}}
#' @param x \linkS4class{AmChart}
#' @param y unused
#' @param width \code{charcter}.
#' @param height \code{charcter}.
#' @param background \code{charcter}.
#' @param ... Other properties.
#' @rdname plot.AmChart
#' @export
setMethod(f = "plot", signature = "AmChart",
          definition = function(x, y, width = "100%", height = "500px",
                                background = "#ffffff",...)
          {
            if (length(x@theme)) {
              background <- switch(x@theme,
                                   "light" = "#ffffff",
                                   "patterns" = "#ffffff",
                                   "default" = "#ffffff",
                                   "dark" = "#3f3f4f",
                                   "chalk" = "#282828 url('http://www.amcharts.com/lib/3/patterns/chalk/bg.jpg')",
                                   stop("[plot]: invalid theme"))
            } else {}
            if (exists("backgroundColor", where = listProperties(x))) {
              background <- listProperties(x)[["backgroundColor"]]
            } else {}
            
            if (length(x@subChartProperties)) {
              jsFile <- "amDrillChart"
              data <- list( main = rlist::list.remove( listProperties(x), "subChartProperties" ) ,
                            subProperties = x@subChartProperties, background = background )
            } else {
              jsFile <- switch(x@type,
                               "funnel" = "amFunnelChart",
                               "gantt" = "amGanttChart",
                               "gauge" = "amAngularGauge",
                               "pie" = "amPieChart",
                               "radar" = "amRadarChart",
                               "serial" = "amSerialChart",
                               "stock" = "amStockChart",
                               "xy" = "amXYChart",
                               stop("type error"))
              data <- list(chartData = listProperties(x), background = background)
            }
            
            htmlwidgets::createWidget(
              name = eval(jsFile),
              data,
              width = width,
              height = height,
              package = 'rAmCharts'
            )
          })

