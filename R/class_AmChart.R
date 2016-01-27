#' @include class_AmObject.R
NULL

#' @title AmChart
#' @description Define the AmChart properties.
#' 
#' @slot allLabels \code{list} of \linkS4class{Label}.
#' Example of label object, with all possible properties:
#' label(x = 20, y = 20, text = "this is label", align = "left", size = 12, color = "#CC0000",
#'       alpha = 1, rotation = 0, bold = TRUE, url = "http=//www.amcharts.com")
#' Run \code{api("Label")} for more information.
#' 
#' @slot arrows \code{list} of \linkS4class{GaugeArrow}. Only valid for gauge charts.
#' Run \code{api("GaugeArrow")} for more information.
#' 
#' @slot axes \code{list} of \linkS4class{GaugeAxis} properties.
#' Only valid for gauge charts.
#' Run \code{api("GaugeAxis")} for more information.
#' 
#' @slot balloon \linkS4class{AmBalloon}.
#' Creates the balloons (tooltips) of the chart,
#' It follows the mouse cursor when you roll-over the data items.
#' The framework generates the instances automatically you only need to adjust
#' the appearance to your needs.
#' Run \code{api("AmBalloon")} for more information.
#' 
#' @slot categoryAxis \linkS4class{CategoryAxis}.
#' Read-only. Chart creates category axis itself.
#' If you want to change some properties,
#' you should get this axis from the chart and set properties to this object.
#' 
#' @slot categoryField \code{character}.
#' Category field name tells the chart the name of the field in your dataProvider object
#' which will be used for category axis values.
#' 
#' @slot ChartCursor \linkS4class{ChartCursor}.
#' Cursor of a chart.
#' Run \code{api("ChartCursor")} for more information.
#' 
#' @slot ChartScrollbar \linkS4class{ChartScrollbar}.
#' Chart's scrollbar.
#' Run \code{api("ChartScrollbar")} for more information.
#' 
#' @slot creditsPosition \code{character},
#' specifying position of link to amCharts site.
#' Allowed values are: "top-left", "top-right", "bottom-left" and "bottom-right".
#' 
#' @slot dataProvider \code{data.frame}, containing the data.
#' 
#' @slot graphs \code{list} of \linkS4class{AmGraph}.
#' Creates the visualization of the data in following types: line, column, step line,
#' smoothed line, olhc and candlestick.
#' 
#' @slot graph \linkS4class{AmGraph}.
#' Only valid for Gantt charts.
#' Gant chart actually creates multiple graphs (separate for each segment).
#' Properties of this graph are passed to each of the created graphs
#' - this allows you to control the look of segments.
#' Run \code{api("AmGraph")} for more information.
#' 
#' @slot guides \code{list} of \linkS4class{Guide}.
#' Instead of adding guides to the axes, you can push all of them to this array.
#' In case guide has category or date defined, it will automatically will be assigned to the category axis.
#' Otherwise to first value axis, unless you specify a different valueAxes for the guide.
#' Run \code{api("Guide")} for more information.
#' 
#' @slot legend  \linkS4class{AmLegend}.
#' Legend of a chart.
#' Run \code{api("AmLegend")} for more information.
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
#' Run \code{api("Title")} for more information.
#' 
#' @slot trendLines \code{list} of \linkS4class{TrendLine} objects added to a chart.
#' You can add trend lines to a chart using this list or access already existing trend lines.
#' 
#' @slot type \code{character}.
#' Possible types are: "serial", "pie", "radar", "xy", "radar", "funnel", "gauge", "stock".
#' See details about using argument type.
#' (type map is in development).
#' 
#' @slot valueAxes \code{list} of \linkS4class{ValueAxis}.
#' Chart creates one value axis automatically,
#' so if you need only one value axis, you don't need to create it.
#' Run \code{api("ValueAxis")} for more information.
#' 
#' @slot valueAxis \linkS4class{ValueAxis}.
#' Only valid for Gantt Charts.
#' Set it's type to "date" if your data is date or time based.
#' 
#' @slot listeners \code{list} containining the listeners to add to the object.
#' The list must be named as in the official API. Each element must a character string.
#' Run \code{runShinyExamples()} for examples.
#' 
#' @slot otherProperties \code{list},
#' containing other avalaible properties non coded in the package.
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

#' @title Create an AmChart
#' @description Method for initializing any S4 class provided by the package.
#' 
#' 
#' @param .Object \linkS4class{AmChart}.
#' 
#' @param allLabels \code{list} of \linkS4class{Label}.
#' Example of label object, with all possible properties:
#' label(x = 20, y = 20, text = "this is label", align = "left", size = 12, color = "#CC0000",
#'       alpha = 1, rotation = 0, bold = TRUE, url = "http=//www.amcharts.com")
#' Run \code{api("Label")} for more information.
#' 
#' @param arrows \code{list} of \linkS4class{GaugeArrow}. Only valid for gauge charts.
#' Run \code{api("GaugeArrow")} for more information.
#' 
#' @param axes \code{list} of \linkS4class{GaugeAxis} properties.
#' Only valid for gauge charts.
#' Run \code{api("GaugeAxis")} for more information.
#' 
#' @param balloon \linkS4class{AmBalloon}.
#' Creates the balloons (tooltips) of the chart,
#' It follows the mouse cursor when you roll-over the data items.
#' The framework generates the instances automatically you only need to adjust
#' the appearance to your needs.
#' Run \code{api("AmBalloon")} for more information.
#' 
#' @param categoryAxis \linkS4class{CategoryAxis}.
#' Read-only. Chart creates category axis itself.
#' If you want to change some properties,
#' you should get this axis from the chart and set properties to this object.
#' 
#' @param categoryField \code{character}.
#' Category field name tells the chart the name of the field in your dataProvider object
#' which will be used for category axis values.
#' 
#' @param chartCursor \linkS4class{ChartCursor}.
#' Cursor of a chart.
#' Run \code{api("ChartCursor")} for more information.
#' 
#' @param chartScrollbar \linkS4class{ChartScrollbar}.
#' Chart's scrollbar.
#' Run \code{api("ChartScrollbar")} for more information.
#' 
#' @param creditsPosition \code{character},
#' specifying position of link to amCharts site.
#' Allowed values are: "top-left", "top-right", "bottom-left" and "bottom-right".
#' 
#' @param dataProvider \code{data.frame}, containing the data.
#' 
#' @param graphs \code{list} of \linkS4class{AmGraph}.
#' Creates the visualization of the data in following types: line, column, step line,
#' smoothed line, olhc and candlestick.
#' 
#' @param graph \linkS4class{AmGraph}.
#' Only valid for Gantt charts.
#' Gant chart actually creates multiple graphs (separate for each segment).
#' Properties of this graph are passed to each of the created graphs
#' - this allows you to control the look of segments.
#' Run \code{api("AmGraph")} for more information.
#' 
#' @param guides \code{list} of \linkS4class{Guide}.
#' Instead of adding guides to the axes, you can push all of them to this array.
#' In case guide has category or date defined, it will automatically will be assigned to the category axis.
#' Otherwise to first value axis, unless you specify a different valueAxes for the guide.
#' Run \code{api("Guide")} for more information.
#' 
#' @param legend  \linkS4class{AmLegend}.
#' Legend of a chart.
#' Run \code{api("AmLegend")} for more information.
#' 
#' @param segmentsField \code{character}.
#' Segments field in your data provider.
#' Only valid for Gantt Charts.
#' 
#' @param theme \code{character}.
#' Theme of a chart. Config files of themes can be found in amcharts/themes/ folder.
#' See \url{http://www.amcharts.com/tutorials/working-with-themes/}.
#' 
#' @param titles \code{list} of \linkS4class{Title}.
#' Run \code{api("Title")} for more information.
#' 
#' @param trendLines \code{list} of \linkS4class{TrendLine} objects added to a chart.
#' You can add trend lines to a chart using this list or access already existing trend lines.
#' 
#' @param type \code{character}.
#' Possible types are: "serial", "pie", "radar", "xy", "radar", "funnel", "gauge", "stock".
#' See details about using argument type.
#' (type map is in development).
#' 
#' @param valueAxes \code{list} of \linkS4class{ValueAxis}.
#' Chart creates one value axis automatically,
#' so if you need only one value axis, you don't need to create it.
#' Run \code{api("ValueAxis")} for more information.
#' 
#' @param valueAxis \linkS4class{ValueAxis}.
#' Only valid for Gantt Charts.
#' Set it's type to "date" if your data is date or time based.
#' 
#' @param ... In case of constructor \code{new("AmChart")} or \code{amChart()}
#' Dots represent other properties to set to the \linkS4class{AmChart} object.
#' See \url{http://docs.amcharts.com/3/javascriptstockchart/AmChart}.
#' In case of setters, dots represent properties of the object to add.
#' See examples.
#' 
#' @return (updated) \linkS4class{AmChart} with given properties.
#' 
#' @examples
#' new("AmChart", valueField = "value")
#' 
#' @seealso Refer to \url{http://docs.amcharts.com/3/javascriptcharts/}.
#' @rdname initialize-AmChart
#' @import methods
#' 
setMethod(f = "initialize", signature = "AmChart",
          definition = function(.Object, allLabels, arrows, axes, balloon, categoryAxis,
                                categoryField, chartCursor, chartScrollbar,
                                creditsPosition, dataProvider, graphs, graph,
                                guides, legend, segmentsField, theme,
                                titles, trendLines, type, valueAxes, valueAxis,...)
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
              .Object <- setTheme(.Object, theme)
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
            if (!missing(valueAxis)) {
              .Object <- setValueAxis(.Object, valueAxis)
            } else {}
            validObject(.Object)
            return(.Object)
          })

#' @title Visualize AmChart with show
#' @description Display the object in the console
#' 
#' @examples
#' pipeR::pipeline(
#'   amPieChart(valueField = "value", titleField = "key"),
#'   setDataProvider(data.frame(key = c("FR", "US"), value = c(20,10))),
#'   setExport(position = "bottom-left")
#' )
#' 
#' @import knitr
#' 
setMethod(f = "show", signature = "AmChart",
          definition = function(object)
          {
            if (length(object@type)) {
              chart <- plot(object)
              if (isTRUE(getOption('knitr.in.progress'))) knitr::knit_print(chart)
              else print(chart)
              
            } else {
              print(object)
            }
          })

#' @rdname listProperties-AmObject
#' @examples
#' # --- signature 'AmChart'
#' listProperties(amChart(test = 1))
#' 
setMethod(f = "listProperties", signature = "AmChart",
          definition = function(.Object)
          {
            ls <- callNextMethod()
            if (length(.Object@allLabels)) {
              ls <- rlist::list.append(ls, allLabels = .Object@allLabels)
            } else {}
            if (length(.Object@arrows)) {
              ls <- rlist::list.append(ls, arrows = .Object@arrows)
            } else {}
            if (length(.Object@axes)) {
              ls <- rlist::list.append(ls, axes = .Object@axes)
            } else {}
            if (length(.Object@balloon)) {
              ls <- rlist::list.append(ls, balloon = .Object@balloon)
            } else {}
            if (length(.Object@categoryAxis)) {
              ls <- rlist::list.append(ls, categoryAxis = .Object@categoryAxis)
            } else {}
            if (length(.Object@categoryField)) {
              ls <- rlist::list.append(ls, categoryField = .Object@categoryField)
            } else {}
            if (length(.Object@creditsPosition)) {
              ls <- rlist::list.append(ls, creditsPosition = .Object@creditsPosition)
            } else {}
            if (length(.Object@chartCursor)) {
              ls <- rlist::list.append(ls, chartCursor = .Object@chartCursor)
            } else {}
            if (length(.Object@chartScrollbar)) {
              ls <- rlist::list.append(ls, chartScrollbar = .Object@chartScrollbar)
            } else {}
            if (length(.Object@dataProvider)) {
              ls <- rlist::list.append(ls, dataProvider = .Object@dataProvider)
            } else {}
            if (length(.Object@graphs)) {
              ls <- rlist::list.append(ls, graphs = .Object@graphs)
            } else {}
            if (length(.Object@graph)) {
              ls <- rlist::list.append(ls, graph = .Object@graph)
            } else {}
            if (length(.Object@guides)) {
              ls <- rlist::list.append(ls, guides = .Object@guides)
            } else {}
            if (length(.Object@legend)) {
              ls <- rlist::list.append(ls, legend = .Object@legend)
            } else {}
            if (length(.Object@segmentsField)) {
              ls <- rlist::list.append(ls, segmentsField = .Object@segmentsField)
            } else {}
            if (length(.Object@subChartProperties)) {
              ls <- rlist::list.append(ls, subChartProperties = .Object@subChartProperties)
            } else {}
            if (length(.Object@theme)) {
              ls <- rlist::list.append(ls, theme = .Object@theme)
            } else {}
            if (length(.Object@titles)) {
              ls <- rlist::list.append(ls, titles = .Object@titles)
            } else {}
            if (length(.Object@trendLines)) {
              ls <- rlist::list.append(ls, trendLines = .Object@trendLines)
            } else {}
            if (length(.Object@type)) {
              ls <- rlist::list.append(ls, type = .Object@type)
            } else {}
            if (length(.Object@valueAxes)) {
              ls <- rlist::list.append(ls, valueAxes = .Object@valueAxes)
            } else {}
            if (length(.Object@valueAxis)) {
              ls <- rlist::list.append(ls, valueAxis = .Object@valueAxis)
            } else {}
            return(ls)
          }
)