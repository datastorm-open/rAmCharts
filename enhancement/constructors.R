#' @examples
#' \dontrun{
#' amChart(type = "pie")
#' }
#' @rdname initialize-AmChart
#' @export
#' 
amChart <- function(...)
{
  # "http://www.amcharts.com/lib/3/images/"
  list_call <- list(Class = "AmChart", ...)
  .Object <- do.call(what = "new", list_call)
  return(.Object)
}

#' @details amAngularGaugeChart is a shortcut for instantiating AmChart of type \code{gauge}.
#' @examples
#' \dontrun{
#' amAngularGaugeChart()
#' }
#' @rdname initialize-AmChart
#' @export
amAngularGaugeChart <- function(...)
{
  # object <- amChart(arrows = arrows, titles = titles, axes = axes, type = "gauge", ...)
  # validObject(object)
  list_call <- list(type = "gauge", ...)
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
  list_call <- list(type = "funnel", marginLeft = marginLeft, marginRight = marginRight, ...)
  .Object <- do.call(what = amChart, list_call)
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
amRadarChart <- function(...)
{
  list_call <- list(type = "radar", ...)
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
amSerialChart <- function(...)
{
  list_call <- list(type = "serial", ...)
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
amPieChart <- function(...)
{
  list_call <- list(type = "pie", ...)
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
amGanttChart <- function(...)
{
  list_call <- list(type = "gantt", ...)
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
amXYChart <- function(...)
{
  list_call <- list(type = "xy", ...)
  .Object <- do.call(what = "amChart", list_call)
  return(.Object)
}
