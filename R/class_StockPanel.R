#' @include class_AmChart.R utils_basicClassUnions.R
NULL

#' @title StockPanel class
#' @author datastorm-open
#' 
#' @description StockPanel class creates stock panels (charts).
#' AmStockChart can have multiple Stock panels.
#' @details Run \code{api("StockPanel")} for more information and all avalaible properties.
#' 
#' 
#' @field drawOnAxis \linkS4class{ValueAxis}.
#' Specifies on which value axis user can draw trend lines.
#' Set drawingIconsEnabled to TRUE if you want icons to be visible.
#' First value axis will be used if not set here.
#' You can use a reference to the value axis object or id of value axis.
#' @field stockGraphs \code{list}.
#' Each element must be have been created with stockGraph(*)
#' @field stockLegend \code{list}.
#' Each element must be have been created with stockLegend(*)
#' 
#' @field allLabels \code{list} of \linkS4class{Label}.
#' Example of label object, with all possible properties:
#' label(x = 20, y = 20, text = "this is label", align = "left", size = 12, color = "#CC0000",
#'       alpha = 1, rotation = 0, bold = TRUE, url = "http=//www.amcharts.com").
#' Run \code{api("Label")} for more informations.
#' @field arrows \code{list} of \linkS4class{GaugeArrow}. Only valid for gauge charts.
#' Run \code{api("GaugeArrow")} for more informations.
#' @field axes \code{list} of \linkS4class{GaugeAxis} properties.
#' Only valid for gauge charts.
#' Run \code{api("GaugeAxis")} for more informations.
#' @field balloon \linkS4class{AmBalloon}
#' Creates the balloons (tooltips) of the chart.
#' It follows the mouse cursor when you roll-over the data items.
#' The framework automatically generates the instances you just have to adjust
#' the appearance to your needs.
#' Run \code{api("AmBalloon")} for more informations.
#' @field categoryAxis \linkS4class{CategoryAxis}.
#' Read-only. Chart creates category axis itself.
#' If you want to change some properties,
#' you should get this axis from the chart and set properties to this object.
#' @field categoryField \code{character}.
#' Category field name indicates the chart the name of the field in your dataProvider object
#' which will be used for category axis values.
#' @field ChartCursor \linkS4class{ChartCursor}.
#' Cursor of a chart.
#' Run \code{api("ChartCursor")} for more informations.
#' @field ChartScrollbar \linkS4class{ChartScrollbar}.
#' Chart's scrollbar.
#' Run \code{api("ChartScrollbar")} for more informations.
#' @field creditsPosition \code{character},
#' specifies position of the amCharts' website link.
#' Allowed values are: "top-left", "top-right", "bottom-left" and "bottom-right".
#' @field dataProvider \code{data.frame}, containing the data.
#' @field graphs \code{list} of \linkS4class{AmGraph}.
#' Creates the visualization of the data in following types: line, column, step line,
#' smoothed line, olhc and candlestick.
#' @field graph \linkS4class{AmGraph}.
#' Only valid for Gantt charts.
#' Gant chart actually creates multiple graphs (separate for each segment).
#' Properties of this graph are passed to each of the created graphs
#' - this allows you to control the look of segments.
#' Run \code{api("AmGraph")} for more informations.
#' @field guides \code{list} of \linkS4class{Guide}.
#' Instead of adding guides to the axes, you can push all of them to this array.
#' In case guide has category or date defined, it automatically will be assigned to the category axis.
#' Otherwise to first value axis, unless you specify a different valueAxes for the guide.
#' Run \code{api("Guide")} for more informations.
#' @field legend  \linkS4class{AmLegend}.
#' Chart's legend.
#' Run \code{api("AmLegend")} for more informations.
#' @field segmentsField \code{character}.
#' Segments field in your data provider.
#' Only valid for Gantt Charts.
#' @field subChartProperties \code{list}.
#' Only valid for Drilldown charts.
#' @field theme \code{character}.
#' Chart's theme. Config files of themes can be found in amcharts/themes/ folder.
#' See \url{http://www.amcharts.com/tutorials/working-with-themes/}.
#' @field titles \code{list} of \linkS4class{Title}.
#' Run \code{api("Title")} for more informations.
#' @field trendLines \code{list} of \linkS4class{TrendLine} objects added to a chart.
#' You can add trend lines to a chart using this list or access already existing trend lines.
#' @field type \code{character}.
#' Possible types are: "serial", "pie", "radar", "xy", "radar", "funnel", "gauge", "stock".
#' See details about using argument type.
#' (type map is in development).
#' @field valueAxes \code{list} of \linkS4class{ValueAxis}.
#' Chart creates one value axis automatically,
#' so if you need only one value axis, you don't need to create it.
#' Run \code{api("ValueAxis")} for more informations.
#' @field valueAxis \linkS4class{ValueAxis}.
#' Only valid for Gantt Charts.
#' Set it's type to "date" if your data is date or time based.
#' @field listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must be a character string.
#' Run \code{runShinyExamples()} for examples.
#' @field otherProperties \code{list}
#' containing other avalaible properties not yet implemented in the package.
#' @field value \code{numeric}.
#' 
#' @export
setClass(Class = "StockPanel", contains = "AmChart",
         representation = representation(
           drawOnAxis = "listOrCharacter",
           stockGraphs = "list",
           stockLegend = "list",
           title = "character"))

#' @title Initialize a StockPanel
#' @description Use the constructor to create the object
#' or update an existing one with the setters.
#' 
#' @param .Object \linkS4class{StockPanel}.
#' 
#' @param drawOnAxis \linkS4class{ValueAxis}.
#' Specifies on which value axis user can draw trend lines.
#' Set drawingIconsEnabled to true if you want drawing icons to be visible.
#' First value axis will be used if not set here.
#' You can use a reference to the value axis object or id of value axis.
#' 
#' @param stockGraphs \code{list} of \linkS4class{AmGraph}.
#' Each element must be have been created with stockGraph(*)
#' 
#' @param stockLegend \code{list} of \linkS4class{AmLegend}.
#' Each element must be have been created with stockLegend(*)
#' 
#' @param title A title of a panel. Note, StockLegend should be added in order title to be displayed.
#' 
#' @template amchart_param
#' 
#' @param ... other properties of StockPanel.
#' 
#' @return (updated) \linkS4class{StockPanel} with given properties.
#' 
#' 
#' 
#' @examples
#' new("StockPanel", title = "Volume")
#' 
#' @rdname initialize-StockPanel
#' @export
#' 
setMethod(f = "initialize", signature = "StockPanel",
          definition = function(.Object,
                                allLabels, axes, balloon, categoryAxis,
                                categoryField, chartCursor, chartScrollbar,
                                creditsPosition, dataProvider, graphs, graph,
                                guides, legend, theme, title, titles,
                                trendLines, type, valueAxes,
                                drawOnAxis, stockGraphs, stockLegend,...)
          { 
            # specific setters for "AmChart"
            if (!missing(allLabels)) .Object <- setAllLabels(.Object, allLabels)
            if (!missing(axes)) .Object <- setAxes(.Object, axes)
            if (!missing(balloon)) .Object <- setBalloon(.Object, balloon)
            if (!missing(categoryAxis)) .Object <- setCategoryAxis(.Object, categoryAxis)
            if (!missing(categoryField)) .Object<- setCategoryField(.Object, categoryField)
            if (!missing(creditsPosition)) .Object <- setCreditsPosition(.Object, creditsPosition)
            if (!missing(chartCursor)) .Object <- setChartCursor(.Object, chartCursor)
            if (!missing(chartScrollbar)) .Object <- setChartScrollbar(.Object, chartScrollbar)
            if (!missing(dataProvider)) .Object <- setDataProvider(.Object, dataProvider)
            if (!missing(graphs)) .Object <- setGraphs(.Object, graphs)
            if (!missing(graph)) .Object <- setGraph(.Object, graph)
            if (!missing(guides)) .Object <- setGuides(.Object, guides)
            if (!missing(legend)) .Object <- setLegend(.Object, legend)
            if (!missing(theme)) .Object <- setTheme(.Object, theme)
            if (!missing(title)) .Object@title <- title
            if (!missing(titles)) .Object <- setTitles(.Object, titles)
            if (!missing(trendLines)) .Object <- setTrendLines(.Object, trendLines)
            if (!missing(type)) .Object <- setType(.Object, type)
            if (!missing(valueAxes)) .Object <- setValueAxes(.Object, valueAxes)
            
            # specific setters for "StockPanel"
            if (!missing(drawOnAxis)) .Object <- setDrawOnAxis(.Object, drawOnAxis)
            if (!missing(stockGraphs)) .Object <- setStockGraphs(.Object, stockGraphs)
            if (!missing(stockLegend)) .Object <- setStockLegend(.Object, stockLegend)
            
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####

#' @rdname initialize-StockPanel
#' @examples
#' stockPanel(stockLegend = legend(useGraphSettings = TRUE))
#' @export
stockPanel <- function(...)
{
  # print(as.list(match.call()))
  # list_call <- c(list(Class = "StockPanel"), as.list(match.call())[-1])
  .Object <- do.call(what = new, list(Class = "StockPanel", ...))
  validObject(.Object)
  return(.Object)
}

#' @rdname initialize-StockPanel
#' @examples
#' panel(creditsPosition = "top-right")
#' panel(title = "top-right")
#' @export
panel <- function(...)
{
  do.call(what = stockPanel, list(...))
}

#' @examples
#' stockPanel(drawnOnAxis = "axis1") 
#' @rdname listProperties-AmObject
setMethod(f = "listProperties", signature = "StockPanel",
          definition = function(.Object)
          { 
            ls <- callNextMethod()
            if (length(.Object@drawOnAxis)) ls$drawOnAxis <- .Object@drawOnAxis
            if (length(.Object@stockGraphs)) ls$stockGraphs <- .Object@stockGraphs
            if (length(.Object@stockLegend)) ls$stockLegend <- .Object@stockLegend
            if (length(.Object@title)) ls$title <- .Object@title
            return(ls)
          })