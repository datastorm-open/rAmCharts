#' @include class_AmObject.R
#' @import rlist
#' @import methods
NULL



#' @title AmChart
#' @description Defines the AmChart properties.
#' 
#' @slot allLabels \code{list} of \linkS4class{Label}.
#' Example of a label object, with all possible properties:
#' label(x = 20, y = 20, text = "this is a label", align = "left", size = 12, color = "#CC0000",
#'       alpha = 1, rotation = 0, bold = TRUE, url = "http=//www.amcharts.com").
#' Run \code{api("Label")} for more informations.
#' 
#' @slot arrows \code{list} of \linkS4class{GaugeArrow}. Only valid for gauge charts.
#' Run \code{api("GaugeArrow")} for more informations.
#' 
#' @slot axes \code{list} of \linkS4class{GaugeAxis} properties.
#' Only valid for gauge charts.
#' Run \code{api("GaugeAxis")} for more informations.
#' 
#' @slot balloon \linkS4class{AmBalloon}.
#' Creates the balloons (tooltips) of the chart,
#' It follows the mouse cursor when you roll-over the data items.
#' The framework generates the instances automatically you just have to adjust
#' the appearance to your needs.
#' Run \code{api("AmBalloon")} for more informations.
#' 
#' @slot categoryAxis \linkS4class{CategoryAxis}.
#' Read-only. Chart creates category axis itself.
#' If you want to change some properties,
#' you should get this axis from the chart and set properties to this object.
#' Run \code{api("CategoryAxis")} for more informations.
#' 
#' @slot categoryField \code{character}.
#' Category field name indicates the name of the field in your dataProvider object
#' which will be used for category axis values.
#' 
#' @slot ChartCursor \linkS4class{ChartCursor}.
#' Chart's cursor.
#' Run \code{api("ChartCursor")} for more informations.
#' 
#' @slot ChartScrollbar \linkS4class{ChartScrollbar}.
#' Chart's scrollbar.
#' Run \code{api("ChartScrollbar")} for more informations.
#' 
#' @slot creditsPosition \code{character},
#' specifies position of the amCharts' website link.
#' Allowed values are: "top-left", "top-right", "bottom-left" and "bottom-right".
#' 
#' @slot dataProvider \code{data.frame}, containing the data.
#' 
#' @slot graphs \code{list} of \linkS4class{AmGraph}.
#' Creates the visualization of the data in following types: line, column, step line,
#' smoothed line, olhc and candlestick.
#' Run \code{api("AmGraph")} for more informations.
#' 
#' @slot graph \linkS4class{AmGraph}.
#' Only valid for Gantt charts.
#' Gant chart actually creates multiple graphs (separate for each segment).
#' Properties of this graph are passed to each of the created graphs
#' - this allows you to control the look of segments.
#' Run \code{api("AmGraph")} for more informations.
#' 
#' @slot guides \code{list} of \linkS4class{Guide}.
#' Instead of adding guides to the axes, you can push all of them to this array.
#' In case guide has category or date defined, it will automatically be assigned to the category axis,
#' otherwise to the first value axis, unless you specify a different valueAxes for the guide.
#' Run \code{api("Guide")} for more informations.
#' 
#' @slot legend  \linkS4class{AmLegend}.
#' Legend of a chart.
#' Run \code{api("AmLegend")} for more informations.
#' 
#' @slot segmentsField \code{character}.
#' Segments field in your data provider.
#' Only valid for Gantt Charts.
#' 
#' @slot subChartProperties \code{list}.
#' Only valid for Drilldown charts.
#' 
#' @slot theme \code{character}.
#' Theme of a chart. Config files of themes can be found in amcharts/themes/ folder.
#' See \url{http://www.amcharts.com/tutorials/working-with-themes/}.
#' 
#' @slot titles \code{list} of \linkS4class{Title}.
#' Run \code{api("Title")} for more informations.
#' 
#' @slot trendLines \code{list} of \linkS4class{TrendLine} objects added to a chart.
#' You can add trend lines to a chart using this list or access already existing trend lines.
#' Run \code{api("TrendLine")} for more informations.
#' 
#' @slot type \code{character}.
#' Possible types are: "serial", "pie", "radar", "xy", "radar", "funnel", "gauge", "stock".
#' See details about using argument type.
#' (type map is in development).
#' 
#' @slot valueAxes \code{list} of \linkS4class{ValueAxis}.
#' Chart creates one value axis automatically,
#' so if you need only one value axis, you don't need to create it.
#' Run \code{api("ValueAxis")} for more informations.
#' 
#' @slot valueAxis \linkS4class{ValueAxis}.
#' Only valid for Gantt Charts.
#' Set it's type to "date" if your data is date or time based.
#' Run \code{api("ValueAxis")} for more informations.
#' 
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must be a character string.
#' Run \code{runShinyExamples()} for examples.
#' 
#' @slot otherProperties \code{list}
#' containing other avalaible properties not yet implemented in the package.
#' 
#' @slot value \code{numeric}.
#' 
#' @examples
#' # Run runShinyExamples() for examples.
#' \dontshow{
#' # see available methods
#' showMethods(class="AmChart")
#' }
#' 
#' @details API for plotting AmChart with R.
#' 
#' @seealso \url{http://docs.amcharts.com/3/javascriptcharts/}
#' 
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
           # check property 'type'
           if (length(object@type)) {
             available_type <- c("funnel", "gantt", "gauge", "pie", "radar", "serial", "xy", "stock")
             if (!object@type %in% available_type) {
               stop("[AmChart]: error when changing the type, maybe it is not implemented yet")
             }
           }
           # check property 'creditsPosition'
           if (length(object@creditsPosition)) {
             available_creditsPosition <- c("top-left", "top-right", "bottom-left", "bottom-right")
             if (!(object@creditsPosition %in% available_creditsPosition)) {
               stop("[AmChart]: invalid property 'creditsPosition'")
             }
           }
           # check property 'theme'
           if (length(object@theme)) {
             available_theme <- c("none", "light", "dark", "patterns", "chalk")
             if (!object@theme %in% available_theme) {
               stop("[AmChart]: invalid property 'theme'")
             }
           }
         })

#' @title Creates an AmChart
#' @description Method to initialize any S4 class provided by the package.
#'
#' @param .Object \linkS4class{AmChart}.
#' 
#' @param arrows \code{list} of \linkS4class{GaugeArrow}. Only valid for gauge charts.
#' Run \code{api("GaugeArrow")} for more informations.
#' 
#' @param segmentsField \code{character}, 
#' segments field in your data provider.
#' Only valid for Gantt Charts.
#' 
#' @param valueAxis \linkS4class{ValueAxis}.
#' Only valid for Gantt Charts.
#' Set it's type to "date" if your data is date or time based.
#' Run \code{api("ValueAxis")} for more informations.
#' 
#' @param ... In case of constructor \code{new("AmChart")} or \code{amChart()}
#' Dots represent other properties to set to the \linkS4class{AmChart} object.
#' See \url{http://docs.amcharts.com/3/javascriptstockchart/AmChart}.
#' In case of setters, dots represent properties of the object to add.
#' See examples.
#' 
#' @template amchart_param
#' 
#' @return (updated) \linkS4class{AmChart} with given properties.
#' 
#' @examples
#' new("AmChart", valueField = "value")
#' 
#' @seealso Refer to \url{http://docs.amcharts.com/3/javascriptcharts/}.
#' @rdname initialize-AmChart
#' 
setMethod(f = "initialize", signature = "AmChart",
          definition = function(.Object, allLabels, arrows, axes, balloon, categoryAxis,
                                categoryField, chartCursor, chartScrollbar,
                                creditsPosition, dataProvider, graphs, graph,
                                guides, legend, segmentsField, theme,
                                titles, trendLines, type, valueAxes, valueAxis,...)
          {
            if (!missing(allLabels)) .Object <- setAllLabels(.Object, allLabels)
            if (!missing(arrows)) .Object <- setArrows(.Object, arrows)
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
            if (!missing(segmentsField)) .Object@segmentsField <- segmentsField
            if (!missing(theme)) .Object <- setTheme(.Object, theme)
            if (!missing(titles)).Object <- setTitles(.Object, titles)
            if (!missing(trendLines)) .Object <- setTrendLines(.Object, trendLines)
            if (!missing(type)) .Object <- setType(.Object, type)
            if (!missing(valueAxes)) .Object <- setValueAxes(.Object, valueAxes)
            if (!missing(valueAxis)) .Object <- setValueAxis(.Object, valueAxis)
            
            validObject(.Object)
            return(.Object)
          })