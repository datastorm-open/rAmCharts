#' @include AmObject.R
NULL

#' @title AmChart
#' @description This class allow to define the amCharts parameters
#' 
#' @slot allLabels
#' Object of class \code{"list"}. List of Labels properties.
#' See \code{\linkS4class{Label}}.
#' 
#' @slot arrows
#' Object of class \code{"list"}
#' containing object of class \code{\linkS4class{GaugeArrow}}.
#' 
#' @slot axes
#' Object of class \code{"list"}
#' containing object of class \code{\linkS4class{GaugeAxis}}.
#' 
#' @slot balloon
#' Object of class \code{"list"}.
#' List of an \code{\linkS4class{AmBalloon}} class properties.
#' Creates the balloons (tooltips) of the chart,
#' It follows the mouse cursor when you roll-over the data items.
#' The framework generates the instances automatically you only need to adjust
#' the appearance to your needs.
#' 
#' @slot categoryAxis
#' Object of class \code{"list"}.
#' List of a \code{\linkS4class{CategoryAxis}} properties.
#' Read-only. Chart creates category axis itself.
#' If you want to change some properties,
#' you should get this axis from the chart and set properties to this object.
#' 
#' @slot categoryField
#' Object of class \code{"character"}.
#' Category field name tells the chart the name of the field in your dataProvider object
#' which will be used for category axis values.
#' 
#' @slot ChartCursor
#' Object of class \code{"list"}.
#' List of a \code{\linkS4class{ChartCursor}} class properties.
#' Properties of the chart's cursor.
#' 
#' @slot ChartScrollbar
#' Object of class \code{"list"}.
#' List of a \code{\linkS4class{ChartScrollbar}} class properties.
#' Properties of chart's scrollbar.
#' 
#' @slot creditsPosition
#' Object of class \code{"character"},
#' specifying position of link to amCharts site.
#' Allowed values are: top-left, top-right, bottom-left and bottom-right.
#' 
#' @slot dataProvider
#' Object of class \code{"list"}, containing the data.
#' Use providing method toList* to convert a \code{data.frame}.
#' 
#' @slot graphs
#' Object of class \code{list}.  List of AmGraphs properties
#' See \code{\linkS4class{AmGraph}} class.
#' Creates the visualization of the data in following types: line, column, step line,
#' smoothed line, olhc and candlestick.
#' 
#' @slot graph
#' Object of class \code{\linkS4class{AmGraph}}.
#' Graph of a Gantt chart. Gant chart actually creates multiple graphs (separate for each segment).
#' Properties of this graph are passed to each of the created graphs
#' - this allows you to control the look of segments.
#' 
#' @slot guides
#' Object of class \code{list}.
#' Each elemnt must be of class \code{\linkS4class{Guide}}.
#' Instead of adding guides to the axes, you can push all of them to this array.
#' In case guide has category or date defined, it will automatically will be assigned to the category axis.
#' Otherwise to first value axis, unless you specify a different valueAxes for the guide.
#' 
#' @slot legend
#' Object of class \code{"list"}.
#' List of an \code{\linkS4class{AmLegend}} class properties.
#' Properties of chart's legend.
#' 
#' @slot segmentsField
#' Object of class \code{character}.
#' 
#' @slot subChartProperties
#' Object of class \code{list}
#' 
#' @slot theme
#' Object of class \code{character}.
#' Theme of a chart. Config files of themes can be found in amcharts/themes/ folder.
#' 
#' @slot titles
#' Object of class \code{"list"}. List of Titles properties
#' See \code{\linkS4class{Title}} class.
#' 
#' @slot trendLines
#' Object of class \code{"list"}.
#' List of \code{\linkS4class{TrendLine}} objects added to a chart.
#' You can add trend lines to a chart using this list or access already existing trend lines.
#' 
#' @slot type
#' Object of class \code{"character"}.
#' Possible types are: serial, pie, radar,
#' (types xy, radar, funnel, gauge, map, stock. are in development).
#' 
#' @slot valueAxes Object of class \code{"list"}. List of ValueAxes' properties.
#' See \code{\linkS4class{ValueAxis}} class.
#' Chart creates one value axis automatically,
#' so if you need only one value axis, you don't need to create it.
#' 
#' @slot valueAxis
#' Object of class \code{list}.
#' List of Value axis properties for Gantt chart. Set it's type to "date" if your data is date or time based.
#' In case of Value axis for a Gantt chart. Set it's type to "date" if your data is date or time based.
#' 
#' @slot listeners
#' Object of class \code{"list"} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string. See examples for details.
#' 
#' @slot otherProperties
#' Object of class \code{"list"},
#' containing other avalaible properties non coded in the package yet.
#' 
#' @slot value
#' Object of class \code{numeric}.
#' 
#' @examples
#' new("AmChart", type = "serial")
#' 
#' \dontshow{
#' # see available methods
#' showMethods(class="AmChart")
#' }
#' 
#' @details API for plotting AmChart with R
#' @author Dataknowledge
#' @seealso \code{\url{http://docs.amcharts.com/3/javascriptcharts}}
#' @family rAmChart classes
#' @export
setClass(Class = "AmChart", contains = "AmObject",
  representation = representation(
    allLabels = "list",
    arrows = "list",
    axes = "list",
    balloon = "list",
    categoryAxis = "list",
    categoryField = "character",
    chartCursor = "list",
    chartScrollbar = "list",
    creditsPosition = "character",
    dataProvider = "list",
    graphs = "list",
    graph = "list",
    guides = "list",
    legend = "list",
    segmentsField = "character",
    subChartProperties = "list",
    theme = "character",
    titles = "list",
    trendLines = "list",
    type = "character",
    valueAxes = "list",
    valueAxis = "list"
 ),
  validity = function(object)
    {
    if (length(object@type) > 0 && ! object@type %in% c("funnel", "gantt", "gauge", "pie", "radar", "serial", "xy", "stock")) {
      stop("[AmChart]: error when changing the type, maybe it is not implemented yet")
    }
    if (length(object@creditsPosition) && !(object@creditsPosition %in% c("top-left", "top-right", "bottom-left", "bottom-right"))) {
      stop("[AmChart]: invalid property 'creditsPosition'")
    }
  }
)

#' @title Initialize an AmChart
#' @description Method for initializing any S4 class provided by the package.
#' @examples
#' new("AmChart", valueField = "value")
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @export
setMethod(f = "initialize", signature = "AmChart",
          definition = function(.Object, allLabels, arrows, axes, balloon, categoryAxis,
                                categoryField, chartCursor,
                                chartScrollbar, creditsPosition, dataProvider, graphs, graph,
                                guides, legend, segmentsField, theme,
                                titles, trendLines, type, valueAxes, valueAxis,
                                pathToImages = "http://www.amcharts.com/lib/3/images/",...)
          {
            if (!missing(allLabels)) {
              .Object <- setAllLabels(.Object, allLabels)
            } else {}
            if (!missing(arrows)) {
              .Object <- setArrows(.Object, arrows)
            } else {}
            if (!missing(axes)) {
              .Object <- setAxes(.Object, axes)
            } else {}
            if (!missing(balloon)) {
              .Object <- setBalloon(.Object, balloon)
            } else {}
            if (!missing(categoryAxis)) {
              .Object <- setCategoryAxis(.Object, categoryAxis)
            } else {}
            if (!missing(categoryField)) {
              .Object<- setCategoryField(.Object, categoryField)
            } else {}
            if (!missing(creditsPosition)) {
              .Object <- setCreditsPosition(.Object, creditsPosition)
            } else {}
            if (!missing(chartCursor)) {
              .Object <- setChartCursor(.Object, chartCursor)
            } else {}
            if (!missing(chartScrollbar)) {
              .Object <- setChartScrollbar(.Object, chartScrollbar)
            } else {}
            if (!missing(dataProvider)) {
              .Object <- setDataProvider(.Object, dataProvider)
            } else {}
            if (!missing(graphs)) {
              .Object <- setGraphs(.Object, graphs)
            } else {}
            if (!missing(graph)) {
              .Object <- setGraph(.Object, graph)
            } else {}
            if (!missing(guides)) {
              .Object <- setGuides(.Object, guides)
            } else {}
            if (!missing(legend)) {
              .Object <- setLegend(.Object, legend)
            } else {}
            if (!missing(segmentsField)) {
              .Object@segmentsField <- segmentsField
            } else {}
            if (!missing(theme)) {
              .Object@theme <- theme
            } else {}
            if (!missing(titles)) {
              .Object <- setTitles(.Object, titles)
            } else {}
            if (!missing(trendLines)) {
              .Object <- setTrendLines(.Object, trendLines)
            } else {}
            if (!missing(type)) {
              .Object <- setType(.Object, type)
            } else {}
            if (!missing(valueAxes)) {
              .Object <- setValueAxes(.Object, valueAxes)
            } else {}
            if (!missing(valueAxis) > 0) {
              .Object <- setValueAxis(.Object, valueAxis)
            } else {}
            .Object <- setProperties(.Object, pathToImages = pathToImages, ...)
            #, path = "http://www.amcharts.com/lib/3/images/"))
            validObject(.Object)
            return(.Object)
          }
)

#' @title List attributes of an AmChart
#' @description This method lists attributes of an AmChart to fit the API
#' @param .Object: \code{\linkS4class{AmChart}}
#' @details For certain attributes we do not verify if they are NULL, see constructor.
#' @examples
#' library(pipeR)
#' amChart() %>>% setProperties(test = 1) %>>% listProperties
#' @importFrom rlist list.append
setMethod(f = "listProperties", signature = "AmChart",
  definition = function(.Object)
  {
    ls <- callNextMethod()
    if (length(.Object@allLabels) > 0) {
      ls <- rlist::list.append(ls, allLabels = .Object@allLabels)
    } else {}
    if (length(.Object@arrows) > 0) {
      ls <- rlist::list.append(ls, arrows = .Object@arrows)
    } else {}
    if (length(.Object@axes) > 0) {
      ls <- rlist::list.append(ls, axes = .Object@axes)
    } else {}
    if (length(.Object@balloon) > 0) {
      ls <- rlist::list.append(ls, balloon = .Object@balloon)
    } else {}
    if (length(.Object@categoryAxis) > 0) {
      ls <- rlist::list.append(ls, categoryAxis = .Object@categoryAxis)
    } else {}
    if (length(.Object@categoryField) > 0) {
      ls <- rlist::list.append(ls, categoryField = .Object@categoryField)
    } else {}
    if (length(.Object@creditsPosition) > 0) {
      ls <- rlist::list.append(ls, creditsPosition = .Object@creditsPosition)
    } else {}
    if (length(.Object@chartCursor) > 0) {
      ls <- rlist::list.append(ls, chartCursor = .Object@chartCursor)
    } else {}
    if (length(.Object@chartScrollbar) > 0) {
      ls <- rlist::list.append(ls, chartScrollbar = .Object@chartScrollbar)
    } else {}
    if (length(.Object@dataProvider) > 0) {
      ls <- rlist::list.append(ls, dataProvider = .Object@dataProvider)
    } else {}
    if (length(.Object@graphs) > 0) {
      ls <- rlist::list.append(ls, graphs = .Object@graphs)
    } else {}
    if (length(.Object@graph) > 0) {
      ls <- rlist::list.append(ls, graph = .Object@graph)
    } else {}
    if (length(.Object@guides) > 0) {
      ls <- rlist::list.append(ls, guides = .Object@guides)
    } else {}
    if (length(.Object@legend) > 0) {
      ls <- rlist::list.append(ls, legend = .Object@legend)
    } else {}
    if (length(.Object@segmentsField) > 0) {
      ls <- rlist::list.append(ls, segmentsField = .Object@segmentsField)
    } else {}
    if (length(.Object@subChartProperties) > 0) {
      ls <- rlist::list.append(ls, subChartProperties = .Object@subChartProperties)
    } else {}
    if (length(.Object@theme) > 0) {
      ls <- rlist::list.append(ls, theme = .Object@theme)
    } else {}
    if (length(.Object@titles) > 0) {
      ls <- rlist::list.append(ls, titles = .Object@titles)
    } else {}
    if (length(.Object@trendLines) > 0) {
      ls <- rlist::list.append(ls, trendLines = .Object@trendLines)
    } else {}
    if (length(.Object@type) > 0) {
      ls <- rlist::list.append(ls, type = .Object@type)
    } else {}
    if (length(.Object@valueAxes) > 0) {
      ls <- rlist::list.append(ls, valueAxes = .Object@valueAxes)
    } else {}
    if (length(.Object@valueAxis) > 0) {
      ls <- rlist::list.append(ls, valueAxis = .Object@valueAxis)
    } else {}
    return(ls)
  }
)