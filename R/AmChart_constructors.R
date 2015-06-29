#' @include AmChart_getSet.R
NULL

#' @title Constructor for a general AmChart object
#' 
#' @param \code{allLabels}: Object of class \code{"list"}. List of Labels properties.
#' See \code{\linkS4class{Label}}.
#' 
#' @param \code{balloon}: Object of class \code{"list"}.
#' List of an \code{\linkS4class{AmBalloon}} class properties.
#' Creates the balloons ( tooltips ) of the chart,
#' It follows the mouse cursor when you roll-over the data items.
#' The framework generates the instances automatically you only need to adjust the appearance to your needs.
#' 
#' @param \code{categoryAxis}: Object of class \code{"list"}.
#' List of a \code{\linkS4class{CategoryAxis}} properties.
#' Read-only. Chart creates category axis itself.
#' If you want to change some properties,
#' you should get this axis from the chart and set properties to this object.
#' 
#' @param \code{categoryField}: Object of class \code{"character"}.
#' Category field name tells the chart the name of the field in your dataProvider object
#' which will be used for category axis values.
#' 
#' @param \code{ChartCursor}: Object of class \code{"list"}.
#' List of a \code{\linkS4class{ChartCursor}} class properties.
#' Properties of the chart's cursor
#' 
#' @param \code{ChartScrollbar}: Object of class \code{"list"}.
#' List of a \code{\linkS4class{ChartScrollbar}} class properties.
#' Properties of chart's scrollbar.
#' 
#' @param \code{creditsPosition}: Object of class \code{"character"},
#' specifying position of link to amCharts site.
#' Allowed values are: top-left, top-right, bottom-left and bottom-right.
#' 
#' @param \code{dataProvider}: Object of class \code{"list"}, containing the data.
#' 
#' @param \code{graphs}: Object of class \code{list}.  List of AmGraphs properties
#' See \code{\linkS4class{AmGraph} class.
#' Creates the visualization of the data in following types: line, column, step line,
#' smoothed line, olhc and candlestick.}
#' 
#' @param \code{guides}: Object of class \code{list}.  List of Guides properties.
#' See \code{\linkS4class{Guides} class.
#' Instead of adding guides to the axes, you can push all of them to this array.
#' In case guide has category or date defined, it will automatically will be assigned to the category axis.
#' Otherwise to first value axis, unless you specify a different valueAxes for the guide.}
#' 
#' @param \code{legend}: Object of class \code{"list"}.
#' List of an \code{\linkS4class{AmLegend}} class properties.
#' Properties of chart's legend.
#' 
#' @param \code{titles}: Object of class \code{"list"}. List of Titles properties
#' See \code{\linkS4class{Title}} class.
#' 
#' @param \code{trendLines}: Object of class \code{"list"}.
#' List of \code{\linkS4class{trendLine}} objects added to a chart.
#' You can add trend lines to a chart using this list or access already existing trend lines.
#' 
#' @param \code{type}: Object of class \code{"character"}.
#' Possible types are: serial, pie, radar,
#' (types xy, radar, funnel, gauge, map, stock. are in development).
#' 
#' @param \code{valueAxes}: Object of class \code{"list"}. List of ValueAxes' properties.
#' See \code{\linkS4class{ValueAxis}} class.
#' Chart creates one value axis automatically,
#' so if you need only one value axis, you don't need to create it.
#' 
#' @slot \code{valueAxis}: Object of class \code{list}.
#' List of Value axis properties for Gantt chart. Set it's type to "date" if your data is date or time based.
#' 
#' @return An \code{\linkS4class{AmChart}} object
#' 
#' @examples
#' amChart(type = "pie")
#' @family rAmChart class constructors
#' @family \code{\linkS4class{AmChart}} constructors
#' @export
amChart <- function(allLabels,
                    arrows,
                    axes,
                    balloon,
                    categoryAxis,
                    categoryField,
                    chartCursor,
                    chartScrollbar,
                    creditsPosition,
                    dataProvider,
                    graph,
                    graphs,
                    guides,
                    legend,
                    segmentsField,
                    theme,
                    titles,
                    trendLines,
                    type,
                    valueAxes,
                    valueAxis, ...)
{
  object <- new(Class="AmChart", pathToImages = "http://www.amcharts.com/lib/3/images/")
  if( !missing(allLabels) ){
    object <-setAllLabels( object, allLabels )
  }else{}
  if( !missing(arrows) ){
    object <-setArrows( object, arrows )
  }else{}
  if( !missing(axes) ){
    object <-setAxes( object, axes)
  }else{}
  if( !missing(balloon) ){
    object <- setBalloon( object, balloon )
  }else{}
  if( !missing(categoryAxis) ){
    object <- setCategoryAxis( object, categoryAxis )
  }else{}
  if( !missing(categoryField) ){
    object<- setCategoryField( object, categoryField )
  }else{}
  if( !missing(creditsPosition) ){
    object <- setCreditsPosition( object, creditsPosition )
  }else{}
  if( !missing(chartCursor) ){
    object <- setChartCursor( object, chartCursor )
  }else{}
  if( !missing(chartScrollbar) ){
    object <- setChartScrollbar( object, chartScrollbar )
  }else{}
  if( !missing(dataProvider) ){
    object <- setDataProvider( object, dataProvider )
  }else{}
  if( !missing(graph) ){
    object <- setGraph( object, graph )
  }else{}
  if( !missing(graphs) ){
    object <- setGraphs(object, graphs)
  }else{}
  if( !missing(guides) ){
    object <- setGuides(object, guides)
  }else{}
  if( !missing(legend) ){
    object <- setLegend( object, legend )
  }else{}
  if( !missing(segmentsField) ){
    object@segmentsField <- segmentsField
  }else{}
  if(!missing(type)){
    object <- setType( object, type)
  }else{}
  if(!missing(theme)){
    object@theme <- theme
  }else{}
  if(!missing(titles)){
    object <- setTitles(object, titles)
  }else{}
  if(!missing(trendLines)){
    object <- setTrendLines(object, trendLines)
  }else{}
  if( !missing(valueAxes) ){
    object <- setValueAxes(object, valueAxes)
  }else{}
  if( !missing(valueAxis) ){
    object <- setValueAxis(object, valueAxis)
  }else{}
  object@otherProperties <- list(...)
  validObject(object)
  return(object)
}

#' @title amAngularGaugeChart is a shortcut for instantiating AmChart of type \code{gauge}
#' @examples
#' amAngularGaugeChart()
#' @rdname amChart
#' @export
amAngularGaugeChart <- function(arrows, titles, axes, ...)
{
  object <- amChart(arrows = arrows, titles = titles, axes = axes, type = "gauge", ...)
  validObject(object)
  return(object)
}

#' @title amFunnelChart is a shortcut
#' for instantiating AmChart of type \code{funnel}
#' @examples
#' amFunnelChart()
#' @rdname amChart
#' @export
amFunnelChart <- function(dataProvider, ...)
{
  object <- amChart(dataProvider = dataProvider, marginLeft = 10, marginRight = 10,
                    type = "funnel", ...)
  validObject(object)
  return(object)
}

#' @title amRadarChart is a shortcut
#' for instantiating AmChart of type \code{radar}
#' @examples
#' amRadarChart()
#' @rdname amChart
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

#' @title amSerialChart is a shortcut constructor 
#' for instantiating AmChart of type \code{serial}
#' @examples
#' amSerialChart()
#' @rdname amChart
#' @export
amSerialChart <- function(allLabels,
                          balloon,
                          categoryAxis,
                          categoryField,
                          chartCursor,
                          chartScrollbar,
                          creditsPosition,
                          dataProvider,
                          graphs,
                          guides,
                          legend,
                          titles,
                          trendLines,
                          valueAxes, ...)
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

#' @title amPieChart is a shortcut constructor
#' for instantiating AmChart of type \code{pie}
#' @examples
#' amPieChart()
#' @rdname amChart
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

#' @title amGanttChart is a constructor
#' for instantiating AmChart of type \code{gantt}
#' @examples
#' amGanttChart(segmentsField = "segments")
#' @rdname amChart
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

#' @title amXYChart is a shortcut constructor
#' for instantiating AmChart of type \code{xy}
#' @examples
#' amXYChart()
#' @rdname amChart
#' @export
amXYChart <- function(creditsPosition,
                      dataProvider,
                      graphs, ...)
{
  object <- amChart( creditsPosition = creditsPosition, dataProvider = dataProvider,
                     graphs = graphs, type = "xy", ...)
  validObject(object)
  return(object)
}