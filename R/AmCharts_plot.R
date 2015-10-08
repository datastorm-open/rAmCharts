#' @include AmChart.R AmStockChart.R
NULL

setClassUnion(name = "AmCharts", members = c("AmChart", "AmStockChart"))

#' @title PLOTTING METHOD
#' @description Basic method to plot an AmChart 
#' @details Plots an object of class \code{\linkS4class{AmChart}}
#' @param x \linkS4class{AmChart}
#' @param y unused
#' @param width \code{character}.
#' @param height \code{character}.
#' @param background \code{character}.
#' @param ... Other properties.
#' @rdname plot.AmChart
#' @export
setMethod(f = "plot", signature = "AmCharts",
          definition = function(x, y, width = "100%", height = "500px",
                                background = "#ffffff",...)
          {
            theme <- listProperties(x)$theme
            if (length(theme)) {
              background <- switch(theme,
                                   "light" = "#ffffff",
                                   "patterns" = "#ffffff",
                                   "default" = "#ffffff",
                                   "dark" = "#3f3f4f",
                                   "chalk" = "#282828",
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
            
            # Add dependency for eport
            if (exists("chartData", where = data)
                && exists("export", where = data$chartData)
                && data$chartData$export$enabled) {
              export_dep <- htmltools::htmlDependency(
                name = "amcharts_plugins_export",
                version = "3",
                src = c(file = system.file("htmlwidgets/lib/plugins/export", package = "rAmCharts")),
                stylesheet = "export.css",
                script = c("export.min.js", "libs/blob.js/blob.js", "libs/fabric.js/fabric.min.js",
                           "libs/FileSaver.js/FileSaver.min.js", "libs/jszip/jszip.min.js",
                           "libs/pdfmake/pdfmake.min.js", "libs/pdfmake/vfs_fonts.js",
                           "libs/xlsx/xlsx.min.js")
              )
              
              if (length(widget$dependencies) == 0) {
                widget$dependencies <- list()
              } else {}
              
              widget$dependencies[[length(widget$dependencies)+1]] <- export_dep
              
            } else {}
            
            # Add dependency for theme
            if (exists("chartData", where = data)
                 && exists("theme", where = data$chartData)) {
              theme_dep <- htmltools::htmlDependency(
                name = paste0("amcharts_themes_", x@theme),
                version = "3",
                src = c(file = system.file("htmlwidgets/lib/themes", package = "rAmCharts")),
                script = switch(theme,
                                "light" = "light.js",
                                "patterns" = "patterns.js",
                                "default" = "",
                                "dark" = "dark.js",
                                "chalk" = "chalk.js",
                                stop("[plot]: invalid theme")
                )
              )
              
              if (length(widget$dependencies) == 0) {
                widget$dependencies <- list()
              } else {}
              
              widget$dependencies[[length(widget$dependencies) + 1]] <- theme_dep
            }
            widget
          })

