#' @param allLabels \code{list} of \linkS4class{Label}.
#' Example of a label object, with all possible properties:
#' label(x = 20, y = 20, text = "this is a label", align = "left", size = 12, color = "#CC0000",
#'       alpha = 1, rotation = 0, bold = TRUE, url = "http=//www.amcharts.com").
#' Run \code{api("Label")} for more informations.
#' 
#' @param axes \code{list} of \linkS4class{GaugeAxis} properties.
#' Only valid for gauge charts.
#' Run \code{api("GaugeAxis")} for more informations.
#' 
#' @param balloon \linkS4class{AmBalloon}.
#' Creates the balloons (tooltips) of the chart.
#' It follows the mouse cursor when you roll-over the data items.
#' The framework automatically generates the instances you just have to adjust
#' the appearance to your needs.
#' Run \code{api("AmBalloon")} for more informations.
#' 
#' @param categoryAxis \linkS4class{CategoryAxis}.
#' Read-only. Chart creates category axis itself.
#' If you want to change some properties,
#' you should get this axis from the chart and set properties to this object.
#' Run \code{api("CategoryAxis")} for more informations.
#' 
#' @param categoryField \code{character}, 
#' category field name indicates the name of the field in your dataProvider object
#' which will be used for category axis values.
#' 
#' @param chartCursor \linkS4class{ChartCursor}.
#' Chart's cursor.
#' Run \code{api("ChartCursor")} for more informations.
#' 
#' @param chartScrollbar \linkS4class{ChartScrollbar}.
#' Chart's scrollbar.
#' Run \code{api("ChartScrollbar")} for more informations.
#' 
#' @param creditsPosition \code{character}, 
#' specifies position of the amCharts' website link.
#' Allowed values are: "top-left", "top-right", "bottom-left" and "bottom-right".
#' 
#' @param dataProvider \code{data.frame}, containing the data.
#' 
#' @param graphs \code{list} of \linkS4class{AmGraph}.
#' Creates the visualization of the data in following types: line, column, step line,
#' smoothed line, olhc and candlestick.
#' Run \code{api("AmGraph")} for more informations.
#' 
#' @param graph \linkS4class{AmGraph}.
#' Only valid for Gantt charts.
#' Gant chart actually creates multiple graphs (separate for each segment).
#' Properties of this graph are passed to each of the created graphs
#' - this allows you to control the look of segments.
#' Run \code{api("AmGraph")} for more informations.
#' 
#' @param guides \code{list} of \linkS4class{Guide}.
#' Instead of adding guides to the axes, you can push all of them to this array.
#' In case guide has category or date defined, it will automatically be assigned to the category axis,
#' otherwise to the first value axis, unless you specify a different valueAxes for the guide.
#' Run \code{api("Guide")} for more informations.
#' 
#' @param legend  \linkS4class{AmLegend}.
#' Legend of a chart.
#' Run \code{api("AmLegend")} for more informations.
#' 
#' @param theme \code{character}, 
#' theme of a chart. Config files of themes can be found in amcharts/themes/ folder.
#' See \url{http://www.amcharts.com/tutorials/working-with-themes/}.
#' 
#' @param titles \code{list} of \linkS4class{Title}.
#' Run \code{api("Title")} for more informations.
#' 
#' @param trendLines \code{list} of \linkS4class{TrendLine} objects added to the chart.
#' You can add trend lines to a chart using this list or access already existing trend lines.
#' Run \code{api("TrendLine")} for more informations.
#' 
#' @param type \code{character}, 
#' possible types are: "serial", "pie", "radar", "xy", "radar", "funnel", "gauge", "stock".
#' See details about using argument type.
#' (type map is in development).
#' 
#' @param valueAxes \code{list} of \linkS4class{ValueAxis}.
#' Chart creates one value axis automatically,
#' so if you need only one value axis, you don't need to create it.
#' Run \code{api("ValueAxis")} for more informations.
#' 
#' @param valueScrollbar \linkS4class{ChartScrollbar}.
#' Value scrollbar, enables scrolling value axes.
