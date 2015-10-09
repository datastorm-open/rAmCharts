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
            chart_ls <- listProperties(x)
            theme <- chart_ls$theme
            if (length(theme)) {
              background <- switch(theme,
                                   "light" = "#ffffff",
                                   "patterns" = "#ffffff",
                                   "default" = "#ffffff",
                                   "dark" = "#3f3f4f",
                                   "chalk" = "#282828",
                                   stop("[plot]: invalid theme"))
            } else {}
            
            # set background
            if (exists("backgroundColor", where = chart_ls)) {
              background <- chart_ls$backgroundColor
              chart_ls <- rlist::list.remove(chart_ls, "backgroundColor")
            } else {}
            
            # listeners on chart
            if (exists("listeners", where = chart_ls)) {
              listeners <- chart_ls$listeners
              chart_ls <- rlist::list.remove(chart_ls, "listeners")
            } else {
              listeners <- NULL
            }
            
            # listeners on legend
            if (exists("legend", where = chart_ls) &&
                exists("listeners", where = chart_ls$legend)) {
              legend_ls <- chart_ls$legend
              legend_listeners <- legend_ls$listeners
              chart_ls$legend <- rlist::list.remove(legend_ls, "listeners")
            } else {
              legend_listeners <- NULL
            }
            
            # case for drilldown chart
            if (exists("subChartProperties", where = chart_ls)) {
              jsFile <- "amDrillChart"
              chart_ls <- rlist::list.remove(chart_ls, "subChartProperties")
              data <- list(main = chart_ls,
                           subProperties = x@subChartProperties,
                           background = background)
            } else {
              jsFile <- "ramcharts_base"
              data <- list(chartData = chart_ls,
                           background = background,
                           listeners = listeners,
                           legend_listeners = legend_listeners)
            }
            
            widget <- htmlwidgets::createWidget(
              name = eval(jsFile),
              x = data,
              width = width,
              height = height,
              package = 'rAmCharts'
            )
            
            widget <- dependency_chartType(widget, data, x@type)
            widget <- dependency_addExport(widget, data)
            widget <- dependency_addTheme(widget, data, theme)
            
            widget
          })

#' Add dependency for chart type
#' @noRd
dependency_chartType <- function(widget, data, type)
{
  jsFiles <- switch(type,
                   "funnel" = "funnel.js",
                   "gantt" = "gantt.js",
                   "gauge" = "gauge.js",
                   "pie" = "pie.js",
                   "radar" = "radar.js",
                   "serial" = "serial.js",
                   "stock" = "amstock.js",
                   "xy" = "xy.js",
                   stop("type error"))
  
  if (type %in% c("gantt", "stock")) {
    jsFiles <- c("serial.js", jsFiles)
  } else {}

  type_dep <- htmltools::htmlDependency(
    name = paste0("amcharts_type", type),
    version = "3.17.2",
    stylesheet = "style.css",
    src = c(file = system.file("htmlwidgets/lib", package = "rAmCharts")),
    script = jsFiles
  )
  
  if (length(widget$dependencies) == 0) {
    widget$dependencies <- list()
  } else {}
  
  widget$dependencies[[length(widget$dependencies)+1]] <- type_dep
  
  widget
}

#' Add dependency for eport
#' @noRd
dependency_addExport <- function(widget, data)
{
  cond <- exists("chartData", where = data) &&
    exists("export", where = data$chartData) &&
    data$chartData$export$enabled
  
  if (cond) {
    export_dep <- htmltools::htmlDependency(
      name = "amcharts_plugins_export",
      version = "3.17.2",
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
  
  widget
}

#' Add theme
#' @noRd
dependency_addTheme <- function(widget, data, theme)
{
  cond <- exists("chartData", where = data) &&
    exists("theme", where = data$chartData)
  
  if (cond) {
    theme_dep <- htmltools::htmlDependency(
      name = paste0("amcharts_themes_", theme),
      version = "3.17.2",
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
    
  } else {}
  
  widget
}
