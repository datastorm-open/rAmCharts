#' @include class_AmChart.R class_AmStockChart.R
NULL

setClassUnion(name = "AmCharts", members = c("AmChart", "AmStockChart"))

#' @title PLOTTING METHOD
#' @description Basic method to plot an AmChart 
#' @details Plots an object of class \code{\linkS4class{AmChart}}
#' @param x \linkS4class{AmChart}
#' @param y unused.
#' @param width \code{character}.
#' @param height \code{character}.
#' @param background \code{character}.
#' @param ... Other properties.
#' @rdname plot.AmChart
#' @import htmlwidgets
#' @import htmltools
#' @export
setMethod(f = "plot", signature = "AmCharts",
          definition = function(x, y, width = "100%", height = NULL,
                                background = "#ffffff",...)
          {
            chart_ls <- listProperties(x)
            # remove temporary parameter
            chart_ls["RType_"] <- NULL
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
              chart_ls["backgroundColor"] <- NULL
            } else {}
            
            # listeners on chart
            if (exists("listeners", where = chart_ls)) {
              listeners <- chart_ls$listeners
              chart_ls["listeners"] <- NULL
            } else {
              listeners <- NULL
            }
            
            # listeners on axes (GaugeAxis class)
            ls_temp <- substituteMultiListeners(chart_ls, "axes")
            chart_ls <- ls_temp$chart
            axes_listeners <- ls_temp$listeners_ls
            axes_listenersIndices <- ls_temp$indices
            
            # listeners on categoryAxis
            ls_temp <- substituteListener(chart_ls, "categoryAxis")
            chart_ls <- ls_temp$chart
            categoryAxis_listeners <- ls_temp$listeners
            
            # listeners on chartCursor
            ls_temp <- substituteListener(chart_ls, "chartCursor")
            chart_ls <- ls_temp$chart
            chartCursor_listeners <- ls_temp$listeners
            
            # listeners on dataSetSelector
            ls_temp <- substituteListener(chart_ls, "dataSetSelector")
            chart_ls <- ls_temp$chart
            dataSetSelector_listeners <- ls_temp$listeners
            
            # listeners on legend
            ls_temp <- substituteListener(chart_ls, "legend")
            chart_ls <- ls_temp$chart
            legend_listeners <- ls_temp$listeners
            
            # listeners on panels
            ls_temp <- substituteMultiListeners(chart_ls, "panels")
            chart_ls <- ls_temp$chart
            panels_listeners <- ls_temp$listeners_ls
            panels_listenersIndices <- ls_temp$indices
            
            # listeners on periodSelector
            ls_temp <- substituteListener(chart_ls, "periodSelector")
            chart_ls <- ls_temp$chart
            periodSelector_listeners <- ls_temp$listeners
            
            # listeners on valueAxis
            ls_temp <- substituteMultiListeners(chart_ls, "valueAxes")
            chart_ls <- ls_temp$chart
            valueAxes_listeners <- ls_temp$listeners_ls
            valueAxes_listenersIndices <- ls_temp$indices
            
            
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
                           # listeners on chart
                           listeners = listeners,
                           #listeners on properties
                           axes_listeners = axes_listeners,
                           axes_listenersIndices = axes_listenersIndices,
                           categoryAxis_listeners = categoryAxis_listeners,
                           chartCursor_listeners = chartCursor_listeners,
                           dataSetSelector_listeners = dataSetSelector_listeners, 
                           legend_listeners = legend_listeners,
                           panels_listeners = panels_listeners,
                           panels_listenersIndices = panels_listenersIndices,
                           periodSelector_listeners = periodSelector_listeners,
                           valueAxes_listeners = valueAxes_listeners,
                           valueAxes_listenersIndices = valueAxes_listenersIndices)
              
            }
            
            widget <- htmlwidgets::createWidget(
              name = eval(jsFile),
              x = data,
              width = width,
              height = height,
              package = 'rAmCharts'
            )
            
            version <- "3.18.2"
            widget <- dependency_chartType(widget, data, x@type, version)
            widget <- dependency_addExport(widget, data, version)
            widget <- dependency_addTheme(widget, data, theme, version)
            widget <- dependency_addDataLoader(widget, data, version)
            widget <- dependency_addResponsive(widget, data, version)
            style_dep <- htmltools::htmlDependency(
              name = "amcharts_style",
              version = version,
              src = c(file = system.file("htmlwidgets/lib", package = "rAmCharts")),
              stylesheet = "style.css"
            )
            
            widget$dependencies[[length(widget$dependencies)+1]] <- style_dep
            
            widget
          })

#' Add dependency for chart type
#' @noRd
dependency_chartType <- function(widget, data, type, version)
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
    version = version,
    src = c(file = system.file("htmlwidgets/lib", package = "rAmCharts")),
    script = jsFiles
  )
  
  if (length(widget$dependencies) == 0) {
    widget$dependencies <- list()
  } else {}
  
  widget$dependencies[[length(widget$dependencies)+1]] <- type_dep
  widget
}

#' Add dependency for export
#' @noRd
dependency_addExport <- function(widget, data, version)
{
  cond <- exists("chartData", where = data) &&
    exists("export", where = data$chartData) &&
    data$chartData$export$enabled
  
  if (cond) {
    export_dep <- htmltools::htmlDependency(
      name = "amcharts_plugins_export",
      version = version,
      src = c(file = system.file("htmlwidgets/lib/plugins/export", package = "rAmCharts")),
      stylesheet = "export.css",
      script = "export.min.js"
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
dependency_addTheme <- function(widget, data, theme, version)
{
  cond <- exists("chartData", where = data) &&
    exists("theme", where = data$chartData) &&
    (data$chartData$theme != "default")
  
  if (cond) {
    theme_dep <- htmltools::htmlDependency(
      name = paste0("amcharts_themes_", theme),
      version = version,
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

#' Add dataloader feature
#' @noRd
dependency_addDataLoader <- function(widget, data, version)
{
  cond <- exists("chartData", where = data) &&
    exists("dataLoader", where = data$chartData)
  
  if (cond) {
    dataloader_dep <- htmltools::htmlDependency(
      name = "amcharts_dataloader",
      version = version,
      src = c(file = system.file("htmlwidgets/lib/plugins/dataloader", package = "rAmCharts")),
      script = "dataloader.min.js"
    )
    
    if (length(widget$dependencies) == 0) {
      widget$dependencies <- list()
    } else {}
    
    widget$dependencies[[length(widget$dependencies) + 1]] <- dataloader_dep
    
  } else {}
  
  widget
}

#' Add responsive feature
#' @noRd
dependency_addResponsive <- function(widget, data, version)
{
  cond <- exists("chartData", where = data) &&
    exists("responsive", where = data$chartData)
  
  if (cond) {
    responsive_dep <- htmltools::htmlDependency(
      name = "amcharts_responsive",
      version = version,
      src = c(file = system.file("htmlwidgets/lib/plugins/responsive", package = "rAmCharts")),
      script = "responsive.min.js"
    )
    
    if (length(widget$dependencies) == 0) {
      widget$dependencies <- list()
    } else {}
    
    widget$dependencies[[length(widget$dependencies) + 1]] <- responsive_dep
    
  } else {}
  
  widget
}

#' Substitue listeners from a single chart object
#' @param chart \code{list} of chart properties.
#' @param obj \code{character} naming the object.
#' @noRd
substituteListener <- function(chart, obj)
{
  if (exists(obj, where = chart) &&
      exists("listeners", where = chart[[eval(obj)]])) {
    chart_obj <- chart[[eval(obj)]]
    listeners <- chart_obj[["listeners"]]
    chart[[eval(obj)]] <- rlist::list.remove(chart_obj, "listeners")
  } else {
    listeners <- NULL
  }
  return(list(chart = chart, listeners = listeners))
}

#' Substitue listeners from a multiple chart object
#' @param chart \code{list} of chart properties.
#' @param obj \code{character} naming the object.
#' @examples
#' x <- list(valueAxes = list(list(title = "tata"), 
#'                            list(title = "titi"),
#'                            list(title = "tata", listeners = "tocnzj")))
#' 
#' substituteMultiListeners(x, "valueAxes")
#' 
#' #---
#' x <- list(valueAxes = list(list(title = "tata"), 
#'                            list(title = "titi"),
#'                            list(title = "tata")))
#' 
#' substituteMultiListeners(x, "valueAxes")
#' @noRd
substituteMultiListeners <- function(chart, obj)
{
  indices <- NULL
  listeners_ls <- NULL
  if (exists(obj, where = chart)) {
    
    # which element has listener(s) ?
    (cond <- lapply(chart[[eval(obj)]], function(x) "listeners" %in% names(x)))
    indices <- which(unlist(cond))
    
    if (length(indices)) {
      
      # for element that have listener(s)
      listeners_ls <- lapply(indices, function(i) {
        chart_obji <- chart[[eval(obj)]][[i]]
        listeners <- chart_obji[["listeners"]]
        chart[[eval(obj)]][[i]] <<- rlist::list.remove(chart_obji, "listeners")
        return(listeners)
      })
      
      # reformat data for JavaScript
      if (length(indices) == 1) {
        indices <- list(indices)
      } else {}
      
    } else {}
    
  } else {}
  
  return(list(chart = chart, listeners_ls = listeners_ls, indices = indices))
}
