#' @examples
#' \donttest{
#' amChart(type = "pie")
#' }
#' @rdname initialize-AmChart
#' @export
#' 
amChart <- function(allLabels, arrows, axes, balloon, categoryAxis, categoryField,
                    chartCursor, chartScrollbar, creditsPosition, dataProvider,
                    graph, graphs, guides, legend, segmentsField, theme, titles,
                    trendLines, type, valueAxes, valueAxis,...)
{
  # "http://www.amcharts.com/lib/3/images/"
  list_call <- c(list(Class = "AmChart"), as.list(match.call())[-1])
  .Object <- do.call(what = "new", list_call)
  return(.Object)
}

#' @details amAngularGaugeChart is a shortcut for instantiating AmChart of type \code{gauge}.
#' @examples
#' \donttest{
#' amAngularGaugeChart()
#' }
#' @rdname initialize-AmChart
#' @export
amAngularGaugeChart <- function(arrows, titles, axes, ...)
{
  # object <- amChart(arrows = arrows, titles = titles, axes = axes, type = "gauge", ...)
  # validObject(object)
  list_call <- c(list(type = "gauge"), as.list(match.call())[-1])
  .Object <- do.call(what = "amChart", list_call)
  return(.Object)
}

#' @details amFunnelChart is a shortcut
#' for instantiating AmChart of type \code{funnel}.
#' @param marginLeft \code{character}, left margin of the chart.
#' @param marginRight \code{character}, right margin of the chart.
#' @examples
#' \donttest{
#' amFunnelChart(marginLeft = 15)
#' }
#' @rdname initialize-AmChart
#' @export
amFunnelChart <- function(dataProvider, marginLeft = 10, marginRight = 10,...)
{
  list_call <- c(list(type = "funnel"), as.list(match.call())[-1])
  .Object <- do.call(what = "amChart", list_call)
  return(.Object)
}

#' @details amRadarChart is a shortcut
#' for instantiating AmChart of type \code{radar}.
#' @examples
#' \donttest{
#' amRadarChart()@type
#' }
#' @rdname initialize-AmChart
#' @export
amRadarChart <- function(allLabels, balloon, categoryField, creditsPosition, dataProvider,
                         graphs, guides, legend, titles, valueAxes, ...)
{
  list_call <- c(list(type = "radar"), as.list(match.call())[-1])
  .Object <- do.call(what = "amChart", list_call)
  return(.Object)
}

#' @details amSerialChart is a shortcut constructor 
#' for instantiating AmChart of type \code{serial}.
#' @examples
#' \donttest{
#' amSerialChart(creditsPostion = "top-right")
#' }
#' @rdname initialize-AmChart
#' @export
amSerialChart <- function(allLabels, balloon, categoryAxis, categoryField, chartCursor,
                          chartScrollbar, creditsPosition, dataProvider, graphs, guides,
                          legend, titles, trendLines, valueAxes, ...)
{
  list_call <- c(list(type = "serial"), as.list(match.call())[-1])
  .Object <- do.call(what = "amChart", list_call)
  return(.Object)
}

#' @details amPieChart is a shortcut constructor
#' for instantiating AmChart of type \code{pie}.
#' @examples
#' \donttest{
#' amPieChart()
#' }
#' @rdname initialize-AmChart
#' @export
amPieChart <- function(allLabels, balloon, creditsPosition,
                       dataProvider, legend, titles, ...)
{
  list_call <- c(list(type = "pie"), as.list(match.call())[-1])
  .Object <- do.call(what = "amChart", list_call)
  return(.Object)
}

#' @details amGanttChart is a constructor
#' for instantiating AmChart of type \code{gantt}.
#' @examples
#' \donttest{
#' amGanttChart(segmentsField = "segments")
#' }
#' @rdname initialize-AmChart
#' @export
amGanttChart <- function(categoryField, dataProvider, graph,
                         segmentsField, valueAxis, ...)
{
  list_call <- c(list(type = "gantt"), as.list(match.call())[-1])
  .Object <- do.call(what = "amChart", list_call)
  return(.Object)
}

#' @details amXYChart is a shortcut constructor
#' for instantiating AmChart of type \code{xy}.
#' @examples
#' \donttest{
#' amXYChart()
#' }
#' @rdname initialize-AmChart
#' @export
amXYChart <- function(creditsPosition, dataProvider, graphs, ...)
{
  list_call <- c(list(type = "xy"), as.list(match.call())[-1])
  .Object <- do.call(what = "amChart", list_call)
  return(.Object)
}