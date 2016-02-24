#' @include class_AmChart.R utils_basicClassUnions.R
NULL

#' @title StockPanel class
#' @author DataKnowledge
#' 
#' @description StockPanel class creates stock panels (charts).
#' AmStockChart can have multiple Stock panels.
#' @details Run \code{api("StockPanel")} for more information and all avalaible properties.
#' 
#' 
#' @field drawOnAxis \linkS4class{ValueAxis}.
#' Specifies on which value axis user can draw trend lines.
#' Set drawingIconsEnabled to true if you want drawing icons to be visible.
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
#'       alpha = 1, rotation = 0, bold = TRUE, url = "http=//www.amcharts.com")
#' Run \code{api("Label")} for more information.
#' @field arrows \code{list} of \linkS4class{GaugeArrow}. Only valid for gauge charts.
#' Run \code{api("GaugeArrow")} for more information.
#' @field axes \code{list} of \linkS4class{GaugeAxis} properties.
#' Only valid for gauge charts.
#' Run \code{api("GaugeAxis")} for more information.
#' @field balloon \linkS4class{AmBalloon}.
#' Creates the balloons (tooltips) of the chart,
#' It follows the mouse cursor when you roll-over the data items.
#' The framework generates the instances automatically you just have to adjust
#' the appearance to your needs.
#' Run \code{api("AmBalloon")} for more information.
#' @field categoryAxis \linkS4class{CategoryAxis}.
#' Read-only. Chart creates category axis itself.
#' If you want to change some properties,
#' you should get this axis from the chart and set properties to this object.
#' @field categoryField \code{character}.
#' Category field name tells the chart the name of the field in your dataProvider object
#' which will be used for category axis values.
#' @field ChartCursor \linkS4class{ChartCursor}.
#' Cursor of a chart.
#' Run \code{api("ChartCursor")} for more information.
#' @field ChartScrollbar \linkS4class{ChartScrollbar}.
#' Chart's scrollbar.
#' Run \code{api("ChartScrollbar")} for more information.
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
#' Run \code{api("AmGraph")} for more information.
#' @field guides \code{list} of \linkS4class{Guide}.
#' Instead of adding guides to the axes, you can push all of them to this array.
#' In case guide has category or date defined, it will automatically will be assigned to the category axis.
#' Otherwise to first value axis, unless you specify a different valueAxes for the guide.
#' Run \code{api("Guide")} for more information.
#' @field legend  \linkS4class{AmLegend}.
#' Legend of a chart.
#' Run \code{api("AmLegend")} for more information.
#' @field segmentsField \code{character}.
#' Segments field in your data provider.
#' Only valid for Gantt Charts.
#' @field subChartProperties \code{list}.
#' Only valid for Drilldown charts.
#' @field theme \code{character}.
#' Theme of a chart. Config files of themes can be found in amcharts/themes/ folder.
#' See \url{http://www.amcharts.com/tutorials/working-with-themes/}.
#' @field titles \code{list} of \linkS4class{Title}.
#' Run \code{api("Title")} for more information.
#' @field trendLines \code{list} of \linkS4class{TrendLine} objects added to a chart.
#' You can add trend lines to a chart using this list or access already existing trend lines.
#' @field type \code{character}.
#' Possible types are: "serial", "pie", "radar", "xy", "radar", "funnel", "gauge", "stock".
#' See details about using argument type.
#' (type map is in development).
#' @field valueAxes \code{list} of \linkS4class{ValueAxis}.
#' Chart creates one value axis automatically,
#' so if you need only one value axis, you don't need to create it.
#' Run \code{api("ValueAxis")} for more information.
#' @field valueAxis \linkS4class{ValueAxis}.
#' Only valid for Gantt Charts.
#' Set it's type to "date" if your data is date or time based.
#' @field listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must be a character string.
#' Run \code{runShinyExamples()} for examples.
#' @field otherProperties \code{list}
#' containing other avalaible properties not yet coded in the package.
#' @field value \code{numeric}.
#' 
#' @author DataKnowledge
#' @export
setClass(Class = "StockPanel", contains = "AmChart",
          representation = representation(
            drawOnAxis = "listOrCharacter",
            stockGraphs = "list",
            stockLegend = "list"
         ))

#' @title Initialize a StockPanel
#' @description Use the constructor to create the object
#' or update an existing one with the setters.
#' 
#' @param .Object \linkS4class{StockPanel}.
#' @param drawOnAxis \linkS4class{ValueAxis}.
#' Specifies on which value axis user can draw trend lines.
#' Set drawingIconsEnabled to true if you want drawing icons to be visible.
#' First value axis will be used if not set here.
#' You can use a reference to the value axis object or id of value axis.
#' @param stockGraphs \code{list} of \linkS4class{AmGraph}.
#' Each element must be have been created with stockGraph(*)
#' @param stockLegend \code{list} of \linkS4class{AmLegend}.
#' Each element must be have been created with stockLegend(*)
#' @param ... Other properties...
#' 
#' @return (updated) \linkS4class{StockPanel} with given properties.
#' 
#' @examples
#' new("StockPanel")
#' @rdname initialize-StockPanel
#' @export
setMethod(f = "initialize", signature = "StockPanel",
          definition = function(.Object, drawOnAxis, stockGraphs, stockLegend, ...)
          {  
            if(!missing(drawOnAxis)){
              .Object <- setDrawOnAxis(.Object, drawOnAxis)
            }
            if(!missing(stockGraphs)){
              .Object <- setStockGraphs(.Object, stockGraphs)
            }
            if(!missing(stockLegend)){
              .Object <- setStockLegend(.Object, stockLegend)
            }
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          })

# CONSTRUCTOR ####

#' @rdname initialize-StockPanel
#' @examples
#' stockPanel()
#' @export
stockPanel <- function(drawOnAxis, stockGraphs, stockLegend, ...){
  .Object <- new("StockPanel")
  if (!missing(drawOnAxis)) {
    .Object <- setDrawOnAxis(.Object, drawOnAxis)
  } else {}
  if (!missing(stockGraphs)) {
    .Object <- setStockGraphs(.Object, stockGraphs)
  } else {}
  if (!missing(stockLegend)) {
    .Object <- setStockLegend(.Object, stockLegend)
  } else {}
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return(.Object)
}

#' @rdname initialize-StockPanel
#' @examples
#' panel()
#' @export
panel <- function(drawOnAxis, stockGraphs, stockLegend, ...){
  .Object <- new("StockPanel")
  if (!missing(drawOnAxis)) {
    .Object <- setDrawOnAxis(.Object, drawOnAxis)
  } else {}
  if (!missing(stockGraphs)) {
    .Object <- setStockGraphs(.Object, stockGraphs)
  } else {}
  if (!missing(stockLegend)) {
    .Object <- setStockLegend(.Object, stockLegend)
  } else {}
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return(.Object)
}

#' @examples
#' stockPanel(drawnOnAxis = "axis1") 
#' @rdname listProperties-AmObject
setMethod(f = "listProperties", signature = "StockPanel",
           definition = function(.Object)
           { 
             ls <- callNextMethod()
             if (length(.Object@drawOnAxis)) {
               ls <- rlist::list.append(ls, drawOnAxis = .Object@drawOnAxis)
             } else {}
             if (length(.Object@stockGraphs)) {
               ls <- rlist::list.append(ls, stockGraphs = .Object@stockGraphs)
             } else {}
             if (length(.Object@stockLegend)) {
               ls <- rlist::list.append(ls, stockLegend = .Object@stockLegend)
             } else {}
             return(ls)
           })