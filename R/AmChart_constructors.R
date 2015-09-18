#' @include AmChart_getSet.R
NULL

#' @examples
#' amChart(type = "pie")
#' @describeIn initialize-AmChart
#' @export
amChart <- function(allLabels, arrows, axes, balloon, categoryAxis, categoryField,
                    chartCursor, chartScrollbar, creditsPosition, dataProvider,
                    graph, graphs, guides, legend, segmentsField, theme, titles,
                    trendLines, type, valueAxes, valueAxis,...)
{
  # "http://www.amcharts.com/lib/3/images/"
  object <- new(Class="AmChart")
  if (!missing(allLabels)) {
    object <-setAllLabels( object, allLabels )
  } else {}
  if (!missing(arrows)) {
    object <-setArrows( object, arrows )
  } else {}
  if (!missing(axes)) {
    object <-setAxes( object, axes)
  } else {}
  if (!missing(balloon)) {
    object <- setBalloon( object, balloon )
  } else {}
  if (!missing(categoryAxis)) {
    object <- setCategoryAxis( object, categoryAxis )
  } else {}
  if (!missing(categoryField)) {
    object<- setCategoryField( object, categoryField )
  } else {}
  if (!missing(creditsPosition)) {
    object <- setCreditsPosition( object, creditsPosition )
  } else {}
  if (!missing(chartCursor)) {
    object <- setChartCursor( object, chartCursor )
  } else {}
  if (!missing(chartScrollbar)) {
    object <- setChartScrollbar( object, chartScrollbar )
  } else {}
  if (!missing(dataProvider)) {
    object <- setDataProvider( object, dataProvider )
  } else {}
  if (!missing(graph)) {
    object <- setGraph( object, graph )
  } else {}
  if (!missing(graphs)) {
    object <- setGraphs(object, graphs)
  } else {}
  if (!missing(guides)) {
    object <- setGuides(object, guides)
  } else {}
  if (!missing(legend)) {
    object <- setLegend( object, legend )
  } else {}
  if (!missing(segmentsField)) {
    object@segmentsField <- segmentsField
  } else {}
  if (!missing(type)) {
    object <- setType( object, type)
  } else {}
  if (!missing(theme)) {
    object@theme <- theme
  } else {}
  if (!missing(titles)) {
    object <- setTitles(object, titles)
  } else {}
  if (!missing(trendLines)) {
    object <- setTrendLines(object, trendLines)
  } else {}
  if (!missing(valueAxes)) {
    object <- setValueAxes(object, valueAxes)
  } else {}
  if (!missing(valueAxis)) {
    object <- setValueAxis(object, valueAxis)
  } else {}
  object@otherProperties <- rlist::list.append(object@otherProperties, ...)
  validObject(object)
  return(object)
}

#' @details amAngularGaugeChart is a shortcut for instantiating AmChart of type \code{gauge}
#' @examples
#' amAngularGaugeChart()
#' @describeIn initialize-AmChart
#' @export
amAngularGaugeChart <- function(arrows, titles, axes, ...)
{
  object <- amChart(arrows = arrows, titles = titles, axes = axes, type = "gauge", ...)
  validObject(object)
  return(object)
}

#' @details amFunnelChart is a shortcut
#' for instantiating AmChart of type \code{funnel}
#' @examples
#' amFunnelChart(marginLeft = 15)
#' @describeIn initialize-AmChart
#' @export
amFunnelChart <- function(dataProvider, ...)
{
  object <- amChart(dataProvider = dataProvider, marginLeft = 10, marginRight = 10,
                    type = "funnel", ...)
  validObject(object)
  return(object)
}

#' @details amRadarChart is a shortcut
#' for instantiating AmChart of type \code{radar}
#' @examples
#' amRadarChart()
#' @describeIn initialize-AmChart
#' @export
amRadarChart <- function(allLabels,
                         balloon,
                         categoryField,
                         creditsPosition,
                         dataProvider,
                         graphs,
                         guides,
                         legend,
                         titles,
                         valueAxes, ...)
{
  object <- amChart(allLabels = allLabels, balloon = balloon,
                    categoryField = categoryField, creditsPosition = creditsPosition,
                    dataProvider = dataProvider, graphs = graphs, guides = guides,
                    legend = legend, titles = titles, valueAxes = valueAxes, type = "radar",...)
  validObject(object)
  return(object)
}

#' @details amSerialChart is a shortcut constructor 
#' for instantiating AmChart of type \code{serial}
#' @examples
#' amSerialChart(creditsPostion = "top-right")
#' @describeIn initialize-AmChart
#' @export
amSerialChart <- function(allLabels, balloon, categoryAxis, categoryField, chartCursor,
                          chartScrollbar, creditsPosition, dataProvider, graphs, guides,
                          legend, titles, trendLines, valueAxes, ...)
{
  object <- amChart(allLabels = allLabels, balloon = balloon, categoryAxis = categoryAxis,
                    categoryField = categoryField, chartCursor = chartCursor,
                    chartScrollbar = chartScrollbar, creditsPosition = creditsPosition,
                    dataProvider = dataProvider, graphs = graphs, guides = guides, legend = legend,
                    titles = titles, trendLines = trendLines,
                    valueAxes = valueAxes, type = "serial",...)
  validObject(object)
  return(object)
}

#' @details amPieChart is a shortcut constructor
#' for instantiating AmChart of type \code{pie}
#' @examples
#' amPieChart()
#' @describeIn initialize-AmChart
#' @export
amPieChart <- function(allLabels,
                       balloon,
                       creditsPosition,
                       dataProvider,
                       legend,
                       titles, ...)
{
  object <- amChart(allLabels = allLabels, balloon = balloon, creditsPosition = creditsPosition,
                    dataProvider = dataProvider, legend = legend, titles = titles,
                    type = "pie", ...)
  validObject(object)
  return(object)
}

#' @details amGanttChart is a constructor
#' for instantiating AmChart of type \code{gantt}
#' @examples
#' amGanttChart(segmentsField = "segments")
#' @describeIn initialize-AmChart
#' @export
amGanttChart <- function(categoryField,
                         dataProvider,
                         graph,
                         segmentsField,
                         valueAxis,...)
{
  object <- amChart( categoryField = categoryField, dataProvider = dataProvider,
                     graph = graph, segmentsField = segmentsField, valueAxis = valueAxis,
                     type = "gantt", ... )
  validObject(object)
  return(object)
}

#' @details amXYChart is a shortcut constructor
#' for instantiating AmChart of type \code{xy}
#' @examples
#' amXYChart()
#' @describeIn initialize-AmChart
#' @export
amXYChart <- function(creditsPosition, dataProvider, graphs, ...)
{
  object <- amChart(creditsPosition = creditsPosition, dataProvider = dataProvider,
                     graphs = graphs, type = "xy", ...)
  validObject(object)
  return(object)
}