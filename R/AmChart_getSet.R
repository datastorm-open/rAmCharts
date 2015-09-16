#' @include sharedGenerics.R CategoryAxis.R AmGraph.R ValueAxis.R ChartCursor.R ChartScrollbar.R AmLegend.R TrendLine.R Title.R Label.R Guide.R
NULL

# > @allLabels : setters ####

#' @title Setter for allLabels
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param allLabels
#' object of class \code{list}.
#' @param ... Other properties.
#' @return
#' The updater object of class \code{\linkS4class{AmChart}}
#' @examples
#' allLabels <- list(label(text = "balloonText"), label(text = "column"))
#' setAllLabels(.Object = amChart(), allLabels = allLabels)
#' amChart(allLabels = allLabels)
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @rdname setAllLabels
#' @docType methods
#' @export
setGeneric(name = "setAllLabels",
            def = function(.Object, allLabels, ...) { standardGeneric("setAllLabels") } )
#' @describeIn setAllLabels
setMethod(f = "setAllLabels", signature = c("AmChart", "list"),
          definition = function(.Object, allLabels)
          {
            rightClassElements <- prod(sapply(allLabels, function(element) {is(element, "Label")}))
            if (!rightClassElements) {
              stop("[setAllLabels]: each element of allLabels must be of class Label")
            } else{}
            .Object@allLabels <- lapply(allLabels, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @title Setter
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}
#' @param label
#' Object of class \code{\linkS4class{Label}}.
#' @param ...
#' properties of class \code{\linkS4class{Label}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' addLabel(.Object = amChart(), text = "balloonText")
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{Label}} S4 class
#' @rdname addLabel
#' @export
setGeneric(name = "addLabel",
            def = function(.Object, label = NULL, ...) { standardGeneric("addLabel") } )
#' @describeIn addLabel
setMethod(f = "addLabel", signature = c("AmChart"),
          definition = function(.Object, label = NULL, ...)
          {
            if (is.null(label)) {
              label <- label(...)
            } else {}
            .Object@allLabels <- rlist::list.append(.Object@allLabels, listProperties(label))
            validObject(.Object)
            return(.Object)
          })

# > @arrows : setters ####

#' @title Setter for arrows
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param arrows
#' (optionnal) list of \code{\linkS4class{GaugeArrow}}.
#' @examples
#' library(pipeR)
#' # Setter for arrows
#' amChart() %>>% setArrows()
#' arrows <- list( gaugeArrow(value = 130), gaugeArrow(value = 150) )
#' amChart() %>>% setArrows(arrows)
#' amChart(arrows = arrows)
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @rdname setArrows
#' @export
setGeneric(name = "setArrows",
            def = function(.Object, arrows = NULL) { standardGeneric("setArrows") } )
#' @describeIn setArrows
setMethod(f = "setArrows", signature = c("AmChart"),
          definition = function(.Object, arrows = NULL)
          {
            if (is.null(arrows)) {
              .Object@arrows <- list(listProperties(gaugeArrow()))
            } else {
              rightClassElements <- prod(sapply(arrows, function(element) {is(element, "GaugeArrow")}))
              if (!rightClassElements) {
                stop("[setArrows]: each element of arrows must be of class GaugeArrow")
              } else {}
              .Object@arrows <- lapply(arrows, listProperties)
            }
            validObject(.Object)
            return(.Object)
          })

#' @title Setter
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}
#' @param arrow
#' (optional) Object of class \code{\linkS4class{GaugeArrow}}.
#' @param ...
#' properties of \code{\linkS4class{GaugeArrow}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.

#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{GaugeArrow}} S4 class
#' @rdname addArrow
#' @examples
#' addArrow(.Object = amChart(), alpha = 1)
#' @export
setGeneric(name = "addArrow",
            def = function(.Object, arrow = NULL, ...) { standardGeneric("addArrow") } )
#' @describeIn addArrow
setMethod(f = "addArrow", signature = c("AmChart"),
           definition = function(.Object, arrow = NULL, ...)
           {
             if (is.null(arrow) && !missing(...)) {
               arrow <- gaugeArrow(...)
             } else {}
             .Object@arrows <- rlist::list.append(.Object@arrows, listProperties(arrow))
             validObject(.Object)
             return(.Object)
           })

# > @axes : setters ####

#' @title Setter for axes
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}
#' @param axes
#' object of class \code{\linkS4class{GaugeAxis}}
#' @param ... Other properties
#' @examples
#' axes <- list(gaugeAxis(value = 130), gaugeAxis(value = 150))
#' setAxes(.Object = amChart(), axes = axes)
#' amChart(axes = axes)
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @rdname setAxes
#' @export
setGeneric(name = "setAxes",
            def = function(.Object, axes, ...) {standardGeneric("setAxes")})
#' @describeIn setAxes
setMethod(f = "setAxes", signature = c("AmChart", "list"),
          definition = function(.Object, axes)
          {
            rightClassElements <- prod(sapply(axes, function(element) {is(element, "GaugeAxis")}))
            if (!rightClassElements) {
              stop("[setAxes]: each element of axes must be of class GaugeAxis")
            } else{}
            .Object@axes <- lapply(axes, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @title Setter
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}
#' @param axe
#' object of class \code{\linkS4class{GaugeAxis}}.
#' @param ...
#' properties of \code{\linkS4class{GaugeAxis}}
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' addAxe(.Object = amChart(), bandAlpha = 1)
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{GaugeAxis}} S4 class
#' @rdname addAxe
#' @export
setGeneric(name = "addAxe",
            def = function(.Object, axe = NULL, ...) {standardGeneric("addAxe")})
#' @describeIn addAxe
setMethod(f = "addAxe", signature = c("AmChart"),
          definition = function(.Object, axe = NULL, ...)
          {
            if (is.null(axe) && !missing(...)) {
              axe <- gaugeAxis(...)
            } else {}
            .Object@axes <- rlist::list.append(.Object@axes, listProperties(axe))
            validObject(.Object)
            return(.Object)
          })

# > @balloon: setters ####

#' @title Setter
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param amBalloon
#' object of class \code{\linkS4class{AmBalloon}}.
#' @param ...
#' properties of \code{\linkS4class{AmBalloon}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' setBalloon(.Object = amChart(), adjustBorderColor = TRUE)
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{AmBalloon}} S4 class
#' @rdname setBalloon
#' @export
setGeneric(name = "setBalloon",
            def = function(.Object, amBalloon = NULL, ...) {standardGeneric("setBalloon")} )
#' @describeIn setBalloon
setMethod(f = "setBalloon", signature = c("AmChart"),
           definition = function(.Object, amBalloon = NULL, ...)
           {
             if (is.null(amBalloon)) {
               amBalloon <- amBalloon(...)
             } else {}
             .Object@balloon <- listProperties(amBalloon)
             validObject(.Object)
             return(.Object)
           })

# > @categoryAxis: setters ####

#' @title Setter
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param categoryAxis
#' object of class \code{\linkS4class{CategoryAxis}}.
#' @param ...
#' properties of \code{\linkS4class{CategoryAxis}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' # Setter for categoryAxis
#' amChart() %>>% setCategoryAxis(categoryAxis(gridPosition = "start"))
#' amChart() %>>% setCategoryAxis(gridPosition = "start")
#' \dontrun{
#' # The argument categoryAxis must be an object of class CategoryAxis
#' amChart() %>>% setCategoryAxis(categoryAxis = "error")
#' }
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{CategoryAxis}} S4 class
#' @rdname setCategoryAxis
#' @export
setGeneric(name = "setCategoryAxis",
            def = function(.Object, categoryAxis = NULL , ...) {standardGeneric("setCategoryAxis")} )
#' @describeIn setCategoryAxis
setMethod(f = "setCategoryAxis", signature = c("AmChart"),
          definition = function(.Object, categoryAxis = NULL, ...)
          {
            if (is.null(categoryAxis)) {
              categoryAxis <- categoryAxis(...)
            } else {}
            .Object@categoryAxis <- listProperties(categoryAxis)
            validObject(.Object)
            return(.Object)
          })

# > @categoryField: setters ####

#' @title Setter
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}
#' @param categoryField
#' object of class \code{category}
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' setCategoryField(.Object = amChart(), categoryField = "category")
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @rdname setCategoryField
#' @export
setGeneric(name = "setCategoryField",
            def = function(.Object, categoryField) {standardGeneric("setCategoryField")} )
#' @describeIn setCategoryField
setMethod(f = "setCategoryField", signature = c("AmChart", "character"),
          definition = function(.Object, categoryField)
          {
            .Object@categoryField <- categoryField
            validObject(.Object)
            return(.Object)
          })

# > @chartCursor : setters ####

#' @title Setter
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param chartCursor
#' Object of class \code{\linkS4class{ChartCursor}}.
#' @param ...
#' properties of \code{\linkS4class{ChartCursor}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% setChartCursor(chartCursor(oneBallOnly = TRUE))
#' 
#' # same result
#' amChart() %>>% setChartCursor(oneBallOnly = TRUE)
#' object <- amChart() %>>% setChartCursor()
#' chartCursor() %>>% class
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{ChartCursor}} S4 class
#' @rdname setChartCursor
#' @export
setGeneric(name = "setChartCursor",
           def = function(.Object, chartCursor = NULL, ...) { standardGeneric("setChartCursor") } )
#' @describeIn setChartCursor
setMethod(f = "setChartCursor", signature = c("AmChart"),
          definition = function(.Object, chartCursor = NULL, ...)
          {
            if (is.null(chartCursor)) {
              chartCursor <- chartCursor(...)
            } else {}
            .Object@chartCursor <- listProperties(chartCursor)
            validObject(.Object)
            return(.Object)
          })

# > @chartScrollbar : setters ####

#' @title Setter
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}
#' @param chartScrollbar
#' object of class \code{\linkS4class{ChartScrollbar}}.
#' @param ...
#' properties of \code{\linkS4class{ChartScrollbar}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% setChartScrollbar(chartScrollbar(oneBallOnly = TRUE))
#' amChart() %>>% setChartScrollbar()
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{ChartScrollbar}} S4 class
#' @rdname setChartScrollbar
#' @export
setGeneric(name = "setChartScrollbar",
           def = function(.Object, chartScrollbar = NULL, ...) { standardGeneric("setChartScrollbar") } )
#' @describeIn setChartScrollbar
setMethod(f = "setChartScrollbar", signature = c("AmChart"),
          definition = function(.Object, chartScrollbar = NULL, ...)
          {
            if (is.null(chartScrollbar)) {
              chartScrollbar <- chartScrollbar( ... )
            } else {}
            .Object@chartScrollbar <- listProperties(chartScrollbar)
            validObject(.Object)
            return(.Object)
          })

# > @creditsPosition: setters ####

#' @title Setter
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param creditsPosition
#' object of class \code{character}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' setCreditsPosition(.Object = amChart(), creditsPosition = "top-right")
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @rdname setCreditsPosition
#' @export
setGeneric(name = "setCreditsPosition",
            def = function(.Object, creditsPosition) {standardGeneric("setCreditsPosition")} )
#' @describeIn setCreditsPosition
setMethod(f = "setCreditsPosition", signature = c("AmChart", "character"),
          definition = function(.Object, creditsPosition)
          {
            .Object@creditsPosition <- creditsPosition
            validObject(.Object)
            return(.Object)
          })

# > @dataProvider : setters ####

#' @title Setter for dataProvider
#' @param .Object
#' Object of class \code{\linkS4class{AmChart}}.
#' @param dataProvider
#' object of class \code{data.frame}.
#' @param keepNA
#' object of class \code{logical}, default \code{TRUE}.
#' Indicates if \code{NULL} values have to be kept or ignored. 
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% setDataProvider(data.frame(key = c("FR", "US"), value = c(20,10)))
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @rdname setDataProvider
#' @export
setMethod(f = "setDataProvider", signature = c("AmChart", "data.frame"),
           definition = function(.Object, dataProvider, keepNA = TRUE)
           {
             .Object@dataProvider <- toList(dataProvider, keepNA )
             validObject(.Object)
             return(.Object)
           })

# > @export: setters ####

#' @title Setter for export
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param enabled
#' object of class \code{logical}, default \code{TRUE}.
#' Should the export button be shown ?
#' @param ...
#' properties for export
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% setExport()
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @rdname setExport
#' @export
setGeneric(name = "setExport",
            def = function(.Object, enabled = TRUE, ...) { standardGeneric("setExport") } )
#' @describeIn setExport
setMethod(f = "setExport", signature = c("AmChart"),
           definition = function(.Object, enabled = TRUE, ...)
           {
             .Object <- setProperties( .Object, export = list(enabled = enabled, ...) )
             validObject(.Object)
             return(.Object)
           })

# > @graphs : setters ####

#' @title Setter
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}
#' @param graphs
#' Object of class \code{list}.
#' Each element must be an object of class \code{\linkS4class{AmGraph}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' graphs <- list(amGraph(balloonText = "balloonText"), amGraph(type = "column"))
#' setGraphs(.Object = amChart(), graphs = graphs)
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{AmGraph}} S4 class
#' @rdname setGraphs
#' @export
setGeneric(name = "setGraphs",
            def = function(.Object, graphs) { standardGeneric("setGraphs") } )
#' @describeIn setGraphs
setMethod(f = "setGraphs", signature = c("AmChart", "list"),
          definition = function(.Object, graphs)
          {
            rightClassElements <- prod(sapply(graphs, function(element) {is(element, "AmGraph")}))
            stopifnot(as.logical(rightClassElements))
            .Object@graphs <- lapply(graphs, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @title Setter
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param amGraph
#' object of class \code{\linkS4class{AmGraph}}, default \code{NULL}.
#' @param ...
#' properties of \code{\linkS4class{AmGraph}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' addGraph(.Object = amChart(), amGraph = amGraph(balloonText = "balloonText", "type" = "column"))
#' addGraph(.Object = amChart(), balloonText = "balloonText", "type" = "column")
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{AmGraph}} S4 class
#' @rdname addGraph
#' @export
setGeneric(name = "addGraph",
            def = function(.Object, amGraph = NULL, ...) { standardGeneric("addGraph") } )
#' @describeIn addGraph
setMethod(f = "addGraph", signature = c(.Object = "AmChart"),
          definition = function(.Object, amGraph = NULL , ...)
          {
            if (is.null(amGraph) && !missing(...)) {
              amGraph <- amGraph(...)
            } else {}
            if (is(amGraph, "AmGraph")) {
              .Object@graphs <- rlist::list.append(.Object@graphs, listProperties(amGraph))
            } else {}
            validObject(.Object)
            return(.Object)
          })

# > @graph: setters ####

#' @title Setter for graph
#' @details Method to use in case of AmChart of type \code{gantt}.
#' For other type see methods \code{setGraphs} or \code{addGraph}.
#' @param .Object
#' object of class \code{\linkS4class{AmChart}} and type \code{gantt}.
#' @param graph
#' Object of class \code{\linkS4class{AmGraph}}.
#' @param ...
#' properties of \code{\linkS4class{AmGraph}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' setGraph(.Object = amGanttChart())
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @rdname setGraph
#' @export
setMethod(f = "setGraph", signature = c("AmChart"),
          definition = function(.Object, graph = NULL, ...)
          {
            if (is.null(graph) && !missing(...)) {
              graph <- amGraph(...)
            } else if (is.null(graph) && missing(...)) {
              graph <- amGraph(balloonText = "[[value]]")
            } else {}
            .Object@graph <- listProperties(graph)
            validObject(.Object)
            return(.Object)
          })

# > @guides : setters ####

#' @title Setter
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param guides
#' Object of class \code{list}.
#' Each element must be an object of class \code{\linkS4class{Guide}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' guides <- list(guide(fillAlpha = .1), guide(fillAlpha = .5))
#' amChart() %>>% setGuides(guides)
#' amChart(guides = guides)
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{Guide}} S4 class
#' @rdname setGuides
#' @export
setGeneric(name = "setGuides",
           def = function(.Object, guides) { standardGeneric("setGuides") })
#' @describeIn setGuides
setMethod(f = "setGuides", signature = c("AmChart", "list"),
          definition = function(.Object, guides)
          {
            rightClassElements <- prod(sapply(guides, function(element) {is(element, "Guide")}))
            if (as.logical(rightClassElements)) {
              .Object@guides <- lapply(guides, listProperties)
            } else {}
            validObject(.Object)
            return(.Object)
          })

#' @title Setter
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param guide
#' object of class \code{\linkS4class{Guide}}, default \code{NULL}
#' @param ...
#' properties of \code{\linkS4class{Guide}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' addGuide(.Object = amChart(), fillAlpha = .1)
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{Guide}} S4 class
#' @rdname addGuide
#' @export
setMethod(f = "addGuide", signature = c("AmChart"),
          definition = function(.Object, guide = NULL, ...)
          {
            if (is.null(guide) && !missing(...)) {
              guide <- guide(...)
            } else {}
            .Object@guides <- rlist::list.append(.Object@guides, listProperties(guide))
            validObject(.Object)
            return(.Object)
          })

# > @legend : setters ####

#' @title Setter for Legend
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param amLegend
#' object of class \code{\linkS4class{AmLegend}}, default \code{NULL}.
#' @param ...
#' properties of \code{\linkS4class{AmLegend}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' # Without chaining
#' setLegend(.Object = amChart(), amLegend = amLegend(useGraphSettings = TRUE))
#' setLegend(.Object = amChart(), useGraphSettings = TRUE)
#' 
#' # With chaining
#' amChart() %>>% setLegend(useGraphSettings = TRUE)
#' amChart() %>>% setLegend( amLegend(useGraphSettings = TRUE) )
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{AmLegend}} S4 class
#' @rdname setLegend
#' @export
setGeneric(name = "setLegend",
           def = function(.Object, amLegend = NULL, ...) {standardGeneric("setLegend")})
#' @describeIn setLegend
setMethod(f = "setLegend", signature = c("AmChart"),
          definition = function(.Object, amLegend = NULL, ...)
          {
            if (is.null(amLegend) && !missing(...)) {
              amLegend <- amLegend(...)
            } else {}
            .Object@legend <- listProperties(amLegend)
            validObject(.Object)
            return(.Object)
          })

# > @segments: setters ####

#' @title Add a segment to a category of AmChart
#' @details Use this methode in case of an AmChart.
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}
#' @param categoryIDs
#' Object of class \code{numeric}.
#' @param sgts
#' Object of class \code{data.frame}
#' ( or \code{list} of \code{data.frame} for multiple add ).
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amGanttChart(segmentsField = "segments"
#' ) %>>% setDataProvider(data.frame(category = c( "John", "Julia"))
#' ) %>>% addSegment(1, data.frame(start = 7, duration = 2:3, task = c("Task #1", "Task #2"))
#' ) %>>% addSegment(2, data.frame(start = 10, duration = 2:3, task = c("Task #1", "Task #2")))
#' 
#' ls <- list( data.frame(start = 7, duration = 2:3, task = c("Task #1", "Task #2")), 
#' data.frame(start = 10, duration = 2:3, task = c("Task #1", "Task #2")))
#' amGanttChart(segmentsField = "segments"
#' ) %>>% setDataProvider(data.frame(category = c( "John", "Julia")) 
#' ) %>>% addSegment( 1:2,  ls)
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @rdname addSegment
#' @export
setGeneric(name = "addSegment",
           def = function (.Object, categoryIDs, sgts) { standardGeneric("addSegment") })
#' @describeIn addSegment
setMethod(f = "addSegment", signature = c( .Object = "AmChart", categoryIDs = "numeric"),
          definition = function(.Object, categoryIDs, sgts) {
            
            if (prod( categoryIDs %in% 1:length(.Object@dataProvider) ) != 1) {
              stop( "[addSegment]: range of argument categoryIDs must in [", 
                    1 , ":" , length(.Object@dataProvider), "]" )
            } else {}
            
            if (!length(.Object@segmentsField)) {
              stop( "[addSegment]: The property segmentsField must be non NULL" )
            } else { segmentField <- as.character(.Object@segmentsField) }
            
            add <- function(.Object, categoryID, sgt) {
              
              if (is(sgt, "data.frame")) {
                #cat("data.frame")
                .Object@dataProvider[[eval(categoryID)]][eval(segmentField)] <<- list(toList(sgt))
              } else if ( is(sgt, "list")) {
                #cat("list")
                .Object@dataProvider[[eval(categoryID)]][eval(segmentField)] <<- sgt
              } else{}
              # print( .Object@dataProvider [[ eval(categoryID) ]] [ eval(segmentField) ] )
              return( .Object )
            }
            
            if (length(categoryIDs) == 1) {
              .Object <- add(.Object, categoryIDs, eval(sgts) )
            } else if (is( sgts, "list" )) {
              invisible(
                sapply(categoryIDs, FUN = function(categoryID) {
                  .Object <<- add(.Object, categoryID, eval(sgts) [[categoryID]])
                })
              )
            } else {}
            validObject(.Object)
            return(.Object)
          })

# > subData for drillChart: setters ####

#' @title Add subData for drilldrown
#' @description This method allows to add subdata for a chart with drilldown.
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param categoryIDs
#' object of class \code{numeric} (vector or simple value).
#' Indicates corresponding indice(s) of the \code{dataProvider} where to add the data.
#' @param data
#' object of class \code{data.frame}. Data to draw at the second level,
#' after clicking on the serial / column.
#' @examples
#' library(pipeR)
#' amChart(dataProvider = data.frame(a = 1:5, b = 6:10)) %>>%
#' addSubData(3, data.frame(a = 1:10, b = 11:20))
#' @rdname addSubData
#' @export
setGeneric(name = "addSubData",
           def = function(.Object, categoryIDs, data) {standardGeneric("addSubData")})
#' @describeIn addSubData
setMethod(f = "addSubData", signature = c("AmChart", "numeric", "data.frame"),
          definition = function(.Object, categoryIDs, data)
          {
            if (prod(categoryIDs %in% 1:length(.Object@dataProvider)) != 1) {
              stop( "[addSubData]: range of argument categoryIDs must in [", 
                    1 , ":" , length(.Object@dataProvider), "]" )
            } else {}
            
            add <- function(.Object, categoryID, data) {
              if (is(data, "data.frame")) {
                #cat("data.frame")
                .Object@dataProvider [[ eval(categoryID) ]] <- rlist::list.append( .Object@dataProvider[[ eval(categoryID) ]],
                                                                                  subdata = toList(data) )
              } else if (is(data, "list")) {
                #cat("list")
                .Object@dataProvider [[ eval(categoryID) ]] <- rlist::list.append( .Object@dataProvider[[ eval(categoryID) ]],
                                                                                   subdata = data )
              } else {}
              return( .Object )
            }
            
            if (length(categoryIDs) == 1) {
              .Object <- add(.Object, categoryIDs, data )
            } else if (is( data, "list" )) {
              invisible(
                sapply(categoryIDs, FUN = function(categoryID) {
                  .Object <<- add(.Object, categoryID, data[[categoryID]])
                })
              )
            } else{}
            validObject(.Object)
            return(.Object)
          })

# > @subChartProperties (for drillChart): setters ####

#' @title Add subData for drilldrown
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param .subObject
#' object of class \code{\linkS4class{AmChart}}.
#' @param ...
#' Properties of \code{\linkS4class{AmChart}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amSerialChart() %>>% setSubChartProperties(type = "pie")
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @rdname setSubChartProperties
#' @export
setGeneric(name = "setSubChartProperties",
           def = function(.Object, .subObject = NULL, ...) {standardGeneric("setSubChartProperties")})
#' @describeIn setSubChartProperties
setMethod(f = "setSubChartProperties", signature = c("AmChart"),
          definition = function(.Object, .subObject = NULL, ...)
          {
            if (is.null(.subObject) && !missing(...)) {
              .subObject <- amChart(...)
            } else {}
            .Object@subChartProperties <- listProperties(.subObject)
            validObject(.Object)
            return(.Object)
          })

# > @titles : setters ####

#' @title Set a list of Title's
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param titles
#' object of class \code{list}.
#' Each element must be an object of class \code{\linkS4class{Title}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' # Setter for titles
#' titles <- list(title(text = "balloonText"), title(text = "column"))
#' amChart() %>>% setTitles(titles)
#' amChart(titles = titles)
#' \dontrun{
#' titles <- list(title(text = "balloonText"), text = "column")
#' amChart(titles = titles)
#' }
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{Title}} S4 class
#' @rdname setTitles
#' @export
setGeneric(name = "setTitles",
            def = function(.Object, titles) { standardGeneric("setTitles") } )
#' @describeIn setTitles
setMethod(f = "setTitles", signature = c("AmChart", "list"),
          definition = function(.Object, titles)
          {
            rightClassElements <- prod(sapply(titles, function(element) {is(element, "Title")}))
            if (!rightClassElements) {
              stop("[setTitles]: each element of setTitles must be of class Title")
            } else {}
            .Object@titles <- lapply(titles, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @title Add a Title
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param title
#' object of class \code{\linkS4class{Title}}, default \code{NULL}.
#' @param ...
#' properties of class \code{\linkS4class{Title}}.
#' @inheritParams amChart
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' addTitle(.Object = amChart(), text = "balloonText")
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{Title}} S4 class
#' @rdname addTitle
#' @export
setGeneric(name = "addTitle",
            def = function(.Object, title = NULL, ...) { standardGeneric("addTitle") } )
#' @describeIn addTitle
setMethod(f = "addTitle", signature = c("AmChart"),
          definition = function(.Object, title = NULL, ...)
          {
            if (is.null(title) && !missing(...)) {
              title <- title(...)
            } else {}
            .Object@titles <- rlist::list.append(.Object@titles, listProperties(title))
            validObject(.Object)
            return(.Object)
          })

# > @trendLines : setters ####

#' @title Setter for trendLines
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param trendLines
#' object of class \code{list}.
#' Each element must be of class \code{\linkS4class{TrendLine}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' trendLines <- list(trendLine(initialValue = 1, finalValue = 5),
#'                    trendLine(initialValue = 7, finalValue = 19))
#' setTrendLines(.Object = amChart(), trendLines = trendLines)
#' amChart(trendLines = trendLines) # Equivalent
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{TrendLine}} S4 class
#' @rdname setTrendLines
#' @export
setGeneric(name = "setTrendLines",
            def = function(.Object, trendLines) { standardGeneric("setTrendLines") } )
#' @describeIn setTrendLines
setMethod(f = "setTrendLines", signature = c("AmChart", "list"),
          definition = function(.Object, trendLines)
          {
            rightClassElements <- prod(sapply(trendLines, function(element) {class(element) == "TrendLine"}))
            stopifnot(as.logical(rightClassElements))
            .Object@trendLines <- lapply(trendLines, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @title Add a trendLine
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param trendLine
#' object of class \code{\linkS4class{TrendLine}}, default \code{NULL}.
#' @param ...
#' properties of class \code{\linkS4class{TrendLine}}.
#' @examples
#' addTrendLine(.Object = amChart(), initialValue = 1, finalValue = 11)
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{TrendLine}} S4 class
#' @rdname addTrendLine
#' @export
setGeneric(name = "addTrendLine",
            def = function(.Object, trendLine = NULL, ...) { standardGeneric("addTrendLine") } )
#' @describeIn addTrendLine
setMethod(f = "addTrendLine", signature = c("AmChart"),
          definition = function(.Object, trendLine = NULL, ...)
          {
            if (is.null(trendLine) && !missing(...)) {
              trendLine <- trendLine(...)
            } else {}
            .Object@trendLines <- rlist::list.append(.Object@trendLines, listProperties(trendLine))
            validObject(.Object)
            return(.Object)
          })

# > @type : setters ####

#' @title Setter for Type
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param type
#' object of class \code{\linkS4class{character}}.
#' @inheritParams amChart
#' @examples
#' setType(.Object = amChart(), type = "pie")
#' # equivalent to
#' amPieChart()
#' @rdname setType
#' @export
setMethod(f = "setType", signature = c("AmChart", "character"),
          definition = function(.Object, type)
          {
            .Object@type <- type
            validObject(.Object)
            return(.Object)
          })


# > @valueAxes : setters ####

#' @title Setter for ValueAxes
#' @description Add a list of ValueAxes objects to an Amchart.
#' @param .Object
#' object of class \code{\linkS4class{AmChart}}.
#' @param valueAxes
#' object of class \code{list}.
#' Each element must be of class \linkS4class{ValueAxis}.
#' @param  ... Properties of \linkS4class{ValueAxis}.
#' @return The updated object of class \linkS4class{AmChart}.
#' @examples
#' valueAxes <- list(valueAxis(axisTitleOffset = 12, tickLength = 10),
#'                   valueAxis(axisTitleOffset = 10, tickLength = 10))
#' setValueAxes(.Object = amChart(), valueAxes = valueAxes)
#' \dontrun{
#' lapply(valueAxes, listProperties)
#' amChart(valueAxes = valueAxes)
#' }
#' @rdname setValueAxes
#' @export
setGeneric(name = "setValueAxes",
           def = function(.Object, valueAxes = NULL, ...) { standardGeneric("setValueAxes") } )
#' @describeIn setValueAxes
setMethod(f = "setValueAxes", signature = c("AmChart", "list"),
          definition = function(.Object, valueAxes)
          {
            rightClassElements <- prod(sapply(valueAxes, function(element) {is(element, "ValueAxis")}))
            stopifnot(as.logical(rightClassElements))
            .Object@valueAxes <- lapply(valueAxes, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @title Add a ValueAxes
#' @param .Object
#' object of class \linkS4class{AmChart}.
#' @param valueAxis
#' object of class \linkS4class{ValueAxis}, default \code{NULL}.
#' @param  ...
#' properties of \linkS4class{ValueAxis}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' addValueAxes(.Object = amChart(), axisTitleOffset = 12, tickLength = 10)
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{ValueAxis}} S4 class
#' @rdname addValueAxes
#' @export
setGeneric(name = "addValueAxes",
            def = function(.Object, valueAxis = NULL, ... ) { standardGeneric("addValueAxes") } )
#' @describeIn addValueAxes
setMethod(f = "addValueAxes", signature = c("AmChart"),
          definition = function(.Object, valueAxis = NULL, ...)
          {
            if (is.null(valueAxis) && !missing(...)) {
              valueAxis <- valueAxis(...)
            } else {}
            .Object@valueAxes <- rlist::list.append(.Object@valueAxes, listProperties(valueAxis))
            validObject(.Object)
            return(.Object)
          })

# > @valueAxis: setters ####

#' @title Setter for valueAxis
#' @param .Object
#' Object of class \code{\linkS4class{AmChart}}
#' with \code{type = "gantt"}.
#' @param valueAxis
#' Object of class \code{\linkS4class{ValueAxis}}, default \code{NULL}.
#' @param  ...
#' properties of \code{\linkS4class{ValueAxis}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' setValueAxis(.Object = amGanttChart())
#' @rdname setValueAxis
#' @export
setMethod(f = "setValueAxis", signature = c("AmChart"),
          definition = function(.Object, valueAxis = NULL, ...)
          {
            if (is.null(valueAxis) && !missing(...)) {
              valueAxis <- valueAxis(...)
            } else if (is.null(valueAxis) && missing(...)) {
              valueAxis <- valueAxis(  autoGridCount = TRUE )
            } else {}
            .Object@valueAxis <- listProperties(valueAxis)
            validObject(.Object)
            return(.Object)
          })
