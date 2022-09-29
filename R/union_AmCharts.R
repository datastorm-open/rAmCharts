#' @include class_AmChart.R class_AmStockChart.R
NULL

setClassUnion(name = "AmCharts", members = c("AmChart", "AmStockChart"))

#' @title Setters for AmChart and AmStockChart.
#' @description These methods can be used both for AmChart and AmStockChart.
#' There are general for some first-level properties.
#' @param .Object \linkS4class{AmChart} or \linkS4class{AmStockChart}.
#' @param enabled \code{logical}, TRUE to display the export button.
#' @param ... Other properties that can be used depending on the setter.
#' @rdname amcharts-setters
#' @export
#' 
setGeneric(name = "setExport", def = function(.Object, enabled = TRUE, ...) {standardGeneric("setExport")})
#' @examples
#' \dontrun{
#' # Dummy examples
#' setExport(amPlot(1:10))
#' setExport(amStockChart())
#' }
#' @rdname amcharts-setters
#' 
setMethod(f = "setExport", signature = c("AmCharts", "logicalOrMissing"),
          definition = function(.Object, enabled = TRUE, ...)
          {
            .Object <- setProperties( .Object, export = list(enabled = enabled, ...) )
            validObject(.Object)
            return(.Object)
          })

#' @rdname amcharts-setters
#' @export
#' 
setGeneric(name = "setResponsive", def = function(.Object, enabled = TRUE, ...) {standardGeneric("setResponsive")})
#' @examples
#' \dontrun{
#' # Dummy examples
#' setResponsive(amSerialChart())
#' setResponsive(amStockChart())
#' }
#' @rdname amcharts-setters
setMethod(f = "setResponsive", signature = c("AmCharts", "logicalOrMissing"),
          definition = function(.Object, enabled = TRUE, ...)
          {
            .Object <- setProperties(.Object = .Object, responsive = list(enabled = enabled, ...))
            validObject(.Object)
            return(.Object)
          })

#' Test wether a chart can be plotted (or printed)
#' @noRd
#' 
.plot_or_print <- function(object)
{
  if (length(object@type)) {
    # cat("Plotting...")
    chart_widget <- plot(object)
    if (isTRUE(getOption('knitr.in.progress')))
      knitr::knit_print(chart_widget)
    else
      print(chart_widget)
  } else {
    # cat("Printing...")
    print(object)
  }
}
#' @title Visualize AmStockChart with show
#' @description Display the object in the console.
#' @param object \linkS4class{AmChart}.
#' @return If the object has a valid type, it will plot the chart.
#' If not the method will trigger the method 'print'.
#' 
setMethod(f = "show", signature = "AmChart", definition = .plot_or_print)
#' @title Visualize AmStockChart with show
#' @description Display the object in the console.
#' @param object \linkS4class{AmStockChart}.
#' @return If the object has a valid type, it will plot the chart.
#' If not the method will trigger the method 'print'.
#' 
setMethod(f = "show", signature = "AmStockChart", definition = .plot_or_print)


#' @title PLOTTING METHOD
#' @description Basic method to plot an AmChart 
#' @details Plots an object of class \code{\linkS4class{AmChart}}
#' @param x \linkS4class{AmChart}
#' @param y unused.
#' @param width \code{character}.
#' @param height \code{character}.B
#' @param background \code{character}.
#' @param ... Other properties.
#' @rdname plot.AmChart
#' @import htmlwidgets
#' @import htmltools
#' @export
setMethod(f = "plot", signature = "AmCharts",
          definition = function(x, y, width = "100%", height = NULL,
                                background = "#ffffff", ...)
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
              chart_ls[grep(x = names(chart_ls), pattern = "^listeners")] <- NULL
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
            
            # listeners on stockLegend
            stockLegend_listeners <- NULL
            if(!is.null(chart_ls$panels) && "stockLegend" %in% names(chart_ls$panels[[1]])){
              if(!is.null(chart_ls$panels[[1]]$stockLegend$listeners)){
                stockLegend_listeners <- chart_ls$panels[[1]]$stockLegend$listeners
                chart_ls$panels[[1]]$stockLegend$listeners <- NULL
              }
            }
            
            # listeners on periodSelector
            ls_temp <- substituteListener(chart_ls, "periodSelector")
            chart_ls <- ls_temp$chart
            periodSelector_listeners <- ls_temp$listeners
            
            # listeners on valueAxis
            ls_temp <- substituteMultiListeners(chart_ls, "valueAxes")
            chart_ls <- ls_temp$chart
            valueAxes_listeners <- ls_temp$listeners_ls
            valueAxes_listenersIndices <- ls_temp$indices
            
            # group (Stock synchronisation)
            if (exists("group", where = chart_ls)) {
              group <- chart_ls$group
              chart_ls[grep(x = names(chart_ls), pattern = "^group")] <- NULL
              if(group == ""){
                group <- NULL
              }
            } else {
              group <- NULL
            }
            
            # is_ts_module (Stock synchronisation & module)
            if (exists("is_ts_module", where = chart_ls)) {
              is_ts_module <- chart_ls$is_ts_module
              chart_ls[grep(x = names(chart_ls), pattern = "^is_ts_module")] <- NULL
            } else {
              is_ts_module <- FALSE
            }
            
            # case for drilldown chart
            if (exists("subChartProperties", where = chart_ls)) {
              
              jsFile <- "amDrillChart"
              chart_ls["subChartProperties"] <- NULL
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
                           valueAxes_listenersIndices = valueAxes_listenersIndices, 
                           stockLegend_listeners = stockLegend_listeners,
                           group = group, is_ts_module = is_ts_module)
              
            }
            
            # Create initial widget
            widget <- htmlwidgets::createWidget(name = eval(jsFile),
                                                x = data,
                                                width = width,
                                                height = height,
                                                package = 'rAmCharts')
            
            # Add dependencies if necessary
            widget <- .add_type_dependency(widget = widget, data = data, type = x@type)
            widget <- .add_export_dependency(widget = widget, data = data)
            widget <- .add_theme_dependency(widget = widget, data = data)
            widget <- .add_dataloader_dependency(widget = widget, data = data)
            widget <- .add_responsive_dependency(widget = widget, data = data)
            widget <- .add_language_dependency(widget = widget, data = data)
            
            return(widget) 
          })

#' Add dependency for chart type
#' @import yaml
#' @noRd
#' 
.add_type_dependency <- function(widget,
                                 data,
                                 type = c("funnel", "gantt", "gauge", "pie",
                                          "radar", "serial", "stock", "xy"))
{
  type <- match.arg(type)
  if (type == "stock") type <- "amstock" # modification temporaire
  file_js <- paste0(type, ".js")
  
  # For some type, we need to source also 'serial.js'
  if (type %in% c("gantt", "amstock")) file_js <- c("serial.js", file_js)
  
  # Load the configuration yaml file into list
  conf_list <- yaml::yaml.load_file(system.file("conf.yaml", package = "rAmCharts"))
  
  # Add main js dependency
  type_dep <- htmltools::htmlDependency(name = paste0("amcharts_type_", type),
                                        # name = paste0("amcharts_type", type),
                                        version = conf_list$amcharts_version,
                                        src = c(file = system.file("htmlwidgets/lib", package = "rAmCharts")),
                                        script = file_js)
  widget <- .add_dependency(widget = widget, dependency = type_dep)
  
  # Add stylesheet if necessary
  if (type == "amstock") {
    style_dep <- htmltools::htmlDependency(name = conf_list$styles$amstockcharts$name,
                                           version = conf_list$amcharts_version,
                                           src = c(file = system.file("htmlwidgets/lib", package = "rAmCharts")),
                                           stylesheet = conf_list$styles$amstockcharts$script)
    widget <- .add_dependency(widget = widget, dependency = style_dep)
  } else {
    # No stylesheet needed
  }
  
  return (widget)
}

#' Add dependency for export
#' @noRd
.add_export_dependency <- function(widget, data)
{
  cond <- exists("chartData", where = data) &&
    exists("export", where = data$chartData) &&
    data$chartData$export$enabled
  
  if (cond) widget <- add_export_dependency(widget)
  
  return (widget)
}


#' @title Add the export dependency to an htmlwidget
#' 
#' @description Add the 'export' dependency to an htmlwidget.
#' You can only manipulate the htmlwidget if you call the method 'plot' with an rAmChart.
#' 
#' @param widget An htmlwidget.
#'
#' @return Return the updated widget with the 'export' dependency.
#' 
#' @export
#' 
add_export_dependency <- function (widget)
{
  # Load the configuration yaml file into list
  conf_list <- yaml::yaml.load_file(system.file("conf.yaml", package = "rAmCharts"))

  # export module
  export_dep <- htmltools::htmlDependency(name = conf_list$plugins$export$name,
                                          version = conf_list$amcharts_version,
                                          src = system.file("htmlwidgets/lib/plugins/export", package = "rAmCharts"),
                                          stylesheet = conf_list$plugins$export$stylesheet,
                                          script = conf_list$plugins$export$script)
  widget <- .add_dependency(widget = widget, dependency = export_dep)

  # blob module
  export_blob_dep <- htmltools::htmlDependency(name = conf_list$plugins$blob$name,
                                          version = conf_list$amcharts_version,
                                          src = system.file("htmlwidgets/lib/plugins/export/libs/blob.js", package = "rAmCharts"),
                                          script = conf_list$plugins$blob$script)
  widget <- .add_dependency(widget = widget, dependency = export_blob_dep)

  # fabric module
  export_fabric_dep <- htmltools::htmlDependency(name = conf_list$plugins$fabric$name,
                                               version = conf_list$amcharts_version,
                                               src = system.file("htmlwidgets/lib/plugins/export/libs/fabric.js", package = "rAmCharts"),
                                               script = conf_list$plugins$fabric$script)
  widget <- .add_dependency(widget = widget, dependency = export_fabric_dep)

  # filesaver module
  export_filesaver_dep <- htmltools::htmlDependency(name = conf_list$plugins$FileSaver$name,
                                               version = conf_list$amcharts_version,
                                               src = system.file("htmlwidgets/lib/plugins/export/libs/FileSaver.js", package = "rAmCharts"),
                                               script = conf_list$plugins$FileSaver$script)
  widget <- .add_dependency(widget = widget, dependency = export_filesaver_dep)

  # jszip module
  export_jszip_dep <- htmltools::htmlDependency(name = conf_list$plugins$jszip$name,
                                               version = conf_list$amcharts_version,
                                               src = system.file("htmlwidgets/lib/plugins/export/libs/jszip", package = "rAmCharts"),
                                               script = conf_list$plugins$jszip$script)
  widget <- .add_dependency(widget = widget, dependency = export_jszip_dep)

  # pdfmake module
  export_pdfmake_dep <- htmltools::htmlDependency(name = conf_list$plugins$pdfmake$name,
                                               version = conf_list$amcharts_version,
                                               src = system.file("htmlwidgets/lib/plugins/export/libs/pdfmake", package = "rAmCharts"),
                                               script = conf_list$plugins$pdfmake$script)
  widget <- .add_dependency(widget = widget, dependency = export_pdfmake_dep)

  export_pdfmake_font_dep <- htmltools::htmlDependency(name = conf_list$plugins$pdfmake_font$name,
                                                  version = conf_list$amcharts_version,
                                                  src = system.file("htmlwidgets/lib/plugins/export/libs/pdfmake", package = "rAmCharts"),
                                                  script = conf_list$plugins$pdfmake_font$script)
  widget <- .add_dependency(widget = widget, dependency = export_pdfmake_font_dep)

  # xlsx module
  export_xlsx_dep <- htmltools::htmlDependency(name = conf_list$plugins$xlsx$name,
                                               version = conf_list$amcharts_version,
                                               src = system.file("htmlwidgets/lib/plugins/export/libs/xlsx", package = "rAmCharts"),
                                               script = conf_list$plugins$xlsx$script)
  widget <- .add_dependency(widget = widget, dependency = export_xlsx_dep)

  # class list
  export_classlist <- htmltools::htmlDependency(name = conf_list$plugins$classList$name,
                                               version = conf_list$amcharts_version,
                                               src = system.file("htmlwidgets/lib/plugins/export/libs/classList.js", package = "rAmCharts"),
                                               script = conf_list$plugins$classList$script)
  widget <- .add_dependency(widget = widget, dependency = export_classlist)
  
  return (widget)
}



#' Add theme
#' @noRd
.add_theme_dependency <- function(widget, data)
{
  cond <- exists("chartData", where = data) &&
    exists("theme", where = data$chartData) &&
    length(data$chartData$theme) &&
    (data$chartData$theme != "default")
  
  if (cond) {
    theme_js <- switch(data$chartData$theme,
                       "light" = "light.js",
                       "patterns" = "patterns.js",
                       "dark" = "dark.js",
                       "chalk" = "chalk.js",
                       stop("[plot]: invalid theme"))
    widget <- add_theme_dependency(widget = widget, theme_js = theme_js)
  } else {
    # Nothing to do, the condition is FALSE
  }
  
  return (widget)
}
#' @title Add theme dependency
#' 
#' @description Add the 'theme' dependency to an htmlwidget.
#' You can only manipulate the htmlwidget if you call the method 'plot' with an rAmChart.
#' 
#' @param widget An htmlwidget.
#' @param theme_js A character indicating the JS file dependency.
#'
#' @return Return the updated htmlwidget.
#' 
#' @examples
#' \dontrun{
#' library(pipeR)
#' amPlot(1:10, theme = "dark") %>>% plot() %>>% add_theme_dependency("light.js")
#' }
#' 
#' @export
#' 
add_theme_dependency <- function (widget, theme_js = c("light.js", "patterns.js", "dark.js", "chalk.js"))
{
  # Load the configuration yaml file into list
  conf_list <- yaml::yaml.load_file(system.file("conf.yaml", package = "rAmCharts"))
  
  theme_dep <- htmltools::htmlDependency(name = paste0("amcharts_themes_", theme_js),
                                         version = conf_list$amcharts_version,
                                         src = system.file("htmlwidgets/lib/themes", package = "rAmCharts"),
                                         script = theme_js)
  widget <- .add_dependency(widget = widget, dependency = theme_dep)
  
  return (widget)
}

#' Add dataloader feature
#' @noRd
.add_dataloader_dependency <- function(widget, data)
{
  cond1 <- exists("chartData", where = data) &&
    exists("dataLoader", where = data$chartData)
  cond2 <- exists("chartData", where = data) &&
    any(sapply(X = data$chartData$dataSets, FUN = exists, x = "dataLoader"))
  
  if (cond1 || cond2) widget <- add_dataloader_dependency(widget = widget)
  
  return(widget)
}

#' @title Add dataloader dependency
#' 
#' @description Add the 'dataloader' dependency to an htmlwidget.
#' You can only manipulate the htmlwidget if you call the method 'plot' with an rAmChart.
#' 
#' @param widget An htmlwidget
#'
#' @return Return the updated htmlwidget.
#' 
#' @export
#'
add_dataloader_dependency <- function(widget)
{
  # Load the configuration yaml file into list
  conf_list <- yaml::yaml.load_file(system.file("conf.yaml", package = "rAmCharts"))
  
  dataloader_dep <- htmltools::htmlDependency(name = conf_list$plugins$dataloader$name,
                                              version = conf_list$amcharts_version,
                                              src = system.file("htmlwidgets/lib/plugins/dataloader", package = "rAmCharts"),
                                              script = conf_list$plugins$dataloader$script)
  widget <- .add_dependency(widget = widget, dependency = dataloader_dep)
  
  return(widget)
}



#' Add responsive feature
#' @noRd
.add_responsive_dependency <- function(widget, data, version)
{
  cond <- exists("chartData", where = data) && exists("responsive", where = data$chartData)
  
  if (cond) widget <- add_responsive_dependency(widget)
  
  return(widget)
}



#' @title Add responsive dependency
#' 
#' @description Add the 'responsive' dependency to an htmlwidget.
#' You can only manipulate the htmlwidget if you call the method 'plot' with an rAmChart.
#' 
#' @param widget An htmlwidget.
#'
#' @return Return an updated htmlwidget with the dependency.
#' 
#' @export
#'
add_responsive_dependency <- function(widget)
{
  # Load the configuration yaml file into list
  conf_list <- yaml::yaml.load_file(system.file("conf.yaml", package = "rAmCharts"))
  
  responsive_dep <- htmltools::htmlDependency(name = conf_list$plugins$responsive$name,
                                              version = conf_list$amcharts_version,
                                              src = system.file("htmlwidgets/lib/plugins/responsive", package = "rAmCharts"),
                                              script = conf_list$plugins$responsive$script)
  widget <- .add_dependency(widget = widget, dependency = responsive_dep)
  
  return(widget)
}

#' @title Add animate dependency
#' 
#' @description Add the 'animate' dependency to an htmlwidget.
#' You can only manipulate the htmlwidget if you call the method 'plot' with an rAmChart.
#' 
#' @param widget An htmlwidget.
#'
#' @return Return an updated htmlwidget with the dependency.
#' 
#' @export
#'
add_animate_dependency <- function(widget)
{
  # Load the configuration yaml file into list
  conf_list <- yaml::yaml.load_file(system.file("conf.yaml", package = "rAmCharts"))
  
  animate_dep <- htmltools::htmlDependency(name = conf_list$plugins$animate$name,
                                              version = conf_list$amcharts_version,
                                              src = system.file("htmlwidgets/lib/plugins/animate", package = "rAmCharts"),
                                              script = conf_list$plugins$animate$script)
  widget <- .add_dependency(widget = widget, dependency = animate_dep)
  
  return(widget)
}

#' @title Add language
#' 
#' @description Add the javascript file associated to the language if necessary
#' 
#' @param widget An htmlwidget.
#' @param data The associated data list.
#'
#' @return Return an updated htmlwidget with the dependency.
#' 
#' @noRd
#' @export
#'
.add_language_dependency <- function(widget, data)
{
  # Load the configuration yaml file into list
  conf_list <- yaml::yaml.load_file(system.file("conf.yaml", package = "rAmCharts"))
  language <- data$chartData$language
  if (length(language) > 0) {
    language_dep_general <- htmltools::htmlDependency(name = "general_language",
                                              version = conf_list$amcharts_version,
                                              src = system.file("htmlwidgets/lib/lang",
                                                                package = "rAmCharts"),
                                              script = paste0(language, ".js"))
    language_dep_export <- htmltools::htmlDependency(name = "export_language",
                                              version = conf_list$amcharts_version,
                                              src = system.file("htmlwidgets/lib/plugins/export/lang",
                                                                package = "rAmCharts"),
                                              script = paste0(language, ".js"))
    widget <- .add_dependency(widget = widget, dependency = language_dep_general)
    widget <- .add_dependency(widget = widget, dependency = language_dep_export)
  } else {
    # no need to add the dependency
  }
  
  return(widget)
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
    chart_obj["listeners"] <- NULL
    chart[[eval(obj)]] <- chart_obj
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
    cond <- lapply(chart[[eval(obj)]], function(x) "listeners" %in% names(x))
    indices <- which(unlist(cond))
    
    if (length(indices)) {
      # for element that have listener(s)
      listeners_ls <- lapply(indices, function(i) {
        chart_obji <- chart[[eval(obj)]][[i]]
        listeners <- chart_obji[["listeners"]]
        chart_obji["listeners"] <- NULL
        chart[[eval(obj)]][[i]] <<- chart_obji
        return(listeners)
      })
      
      # reformat data for JavaScript
      if (length(indices) == 1)
        indices <- list(indices)
    } else {}
    
  } else {}
  
  return(list(chart = chart, listeners_ls = listeners_ls, indices = indices))
}

#' @title Add any dependency to an htmlwidget
#' @param widget An htmlwidget.
#' @param dependency An htmlDependency.
#' @return The widget with the given dependency
#' @noRd
#' 
.add_dependency <- function (widget, dependency)
{
  if (length(widget$dependencies) == 0) widget$dependencies <- list()
  widget$dependencies[[length(widget$dependencies)+1]] <- dependency
  
  return(widget)
}
