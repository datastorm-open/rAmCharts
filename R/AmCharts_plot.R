#' @include AmChart.R AmStockChart.R
NULL

setClassUnion(name = "AmCharts", members = c("AmChart", "AmStockChart"))

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
setMethod(f = "plot", signature = "AmCharts",
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
            
            if (exists("subChartProperties", where = listProperties(x))) {
              jsFile <- "amDrillChart"
              data <- list(main = rlist::list.remove(listProperties(x), "subChartProperties") ,
                            subProperties = x@subChartProperties, background = background)
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
            
            widget <- htmlwidgets::createWidget(
              name = eval(jsFile),
              data,
              width = width,
              height = height,
              package = 'rAmCharts'
            )
            
            
            if (exists("chartData", where = data) && exists("export", where = data$chartData) && data$chartData$export$enabled) {
              export_dep <- htmltools::htmlDependency(
                name = "amcharts_plugins_export",
                version = "3",
                src = c(file = system.file("htmlwidgets/lib/amcharts/plugins/export", package = "rAmCharts")),
                stylesheet = "export.css",
                script = c("export.min.js", "libs/blob.js/blob.js", "libs/fabric.js/fabric.min.js",
                           "libs/FileSaver.js/FileSaver.min.js", "libs/jszip/jszip.min.js",
                           "libs/pdfmake/pdfmake.min.js", "libs/pdfmake/vfs_fonts.js",
                           "libs/xlsx/xlsx.min.js")
              )
              
              if (length(widget$dependencies) == 0) {
                widget$dependencies = list()
              } else {}
              
              widget$dependencies[[length(widget$dependencies)+1]] <- export_dep
              
            } else {}
            
            widget
          })

