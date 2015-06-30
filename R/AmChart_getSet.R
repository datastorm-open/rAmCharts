#' @include sharedGenerics.R CategoryAxis.R AmGraph.R ValueAxis.R ChartCursor.R ChartScrollbar.R AmLegend.R TrendLine.R Title.R Label.R Guide.R
NULL

# > @allLabels : setters ####

#' @exportMethod setAllLabels
setGeneric( name = "setAllLabels",
            def = function(.Object, allLabels, ...) { standardGeneric("setAllLabels") } )
#' @title Setter for allLabels
#' @examples
#' library(pipeR)
#' # Setter for allLabels
#' allLabels <- list(label(text = "balloonText"), label(text = "column"))
#' amChart() %>>% setAllLabels(allLabels)
#' amChart(allLabels = allLabels)
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @name setAllLabels
#' @rdname setAllLabels
#' @export
setMethod( f = "setAllLabels", signature = c("AmChart", "list"),
           definition = function(.Object, allLabels)
           {
             rightClassElements <- prod(sapply(allLabels, function(element) {is(element, "Label")}))
             if( !rightClassElements ){
               stop("[setAllLabels]: each element of allLabels must be of class Label")
             }else{}
             .Object@allLabels <- lapply(allLabels, listProperties)
             validObject(.Object)
             return(.Object)
           }
)

#' @exportMethod addLabel
setGeneric( name = "addLabel",
            def = function(.Object, label = NULL, ...) { standardGeneric("addLabel") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{label}: Object of class \code{\linkS4class{Label}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' # Setter for label
#' library(pipeR)
#' amChart() %>>% addLabel(text = "balloonText")
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{Label}} S4 class
#' @name addLabel
#' @rdname addLabel
#' @importFrom rlist list.append
#' @export
setMethod( f = "addLabel", signature = c("AmChart"),
           definition = function(.Object, label = NULL, ...)
           {
             if( is.null(label) ){
               label <- label(...)
             }else{}
             .Object@allLabels <- rlist::list.append(.Object@allLabels, listProperties(label))
             validObject(.Object)
             return(.Object)
           }
)

# > @arrows : setters ####

#' @exportMethod setArrows
setGeneric( name = "setArrows",
            def = function(.Object, arrows = NULL) { standardGeneric("setArrows") } )
#' @title Setter for arrows
#' @examples
#' library(pipeR)
#' # Setter for arrows
#' amChart() %>>% setArrows()
#' arrows <- list( gaugeArrow(value = 130), gaugeArrow(value = 150) )
#' amChart() %>>% setArrows(arrows)
#' amChart(arrows = arrows)
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @name setArrows
#' @rdname setArrows
#' @export
setMethod( f = "setArrows", signature = c("AmChart"),
           definition = function(.Object, arrows = NULL)
           {
             if( is.null(arrows) ){
               .Object@arrows <- list( listProperties(gaugeArrow()) )
             }else{
               rightClassElements <- prod(sapply(arrows, function(element) {is(element, "GaugeArrow")}))
               if( !rightClassElements ){
                 stop("[setArrows]: each element of arrows must be of class GaugeArrow")
               }else{}
               .Object@arrows <- lapply(arrows, listProperties)
             }
             validObject(.Object)
             return(.Object)
           }
)

#' @exportMethod addArrow
setGeneric( name = "addArrow",
            def = function(.Object, arrow = NULL, ...) { standardGeneric("addArrow") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{arrow}: Object of class \code{\linkS4class{GaugeArrow}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% addArrow(alpha = 1)
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{GaugeArrow}} S4 class
#' @name addArrow
#' @rdname addArrow
#' @importFrom rlist list.append
#' @export
setMethod( f = "addArrow", signature = c("AmChart"),
           definition = function(.Object, arrow = NULL, ...)
           {
             if( is.null(arrow) && !missing(...) ){
               arrow <- gaugeArrow(...)
             }else{}
             .Object@arrows <- rlist::list.append(.Object@arrows, listProperties(arrow))
             validObject(.Object)
             return(.Object)
           }
)

# > @axes : setters ####

#' @exportMethod setAxes
setGeneric( name = "setAxes",
            def = function(.Object, axes, ...) { standardGeneric("setAxes") } )
#' @title Setter for axes
#' @examples
#' library(pipeR)
#' # Setter for axes
#' axes <- list( gaugeAxis(value = 130), gaugeAxis(value = 150) )
#' amChart() %>>% setAxes(axes)
#' amChart(axes = axes)
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @name setAxes
#' @rdname setAxes
#' @export
setMethod( f = "setAxes", signature = c("AmChart", "list"),
           definition = function(.Object, axes)
           {
             rightClassElements <- prod(sapply(axes, function(element) {is(element, "GaugeAxis")}))
             if( !rightClassElements ){
               stop("[setAxes]: each element of axes must be of class GaugeAxis")
             }else{}
             .Object@axes <- lapply(axes, listProperties)
             validObject(.Object)
             return(.Object)
           }
)

#' @exportMethod addAxe
setGeneric( name = "addAxe",
            def = function(.Object, axe = NULL, ...) { standardGeneric("addAxe") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{axe}: Object of class \code{\linkS4class{GaugeAxis}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' # Setter for axe
#' amChart() %>>% addAxe(bandAlpha = 1)
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{GaugeAxis}} S4 class
#' @name addAxe
#' @rdname addAxe
#' @importFrom rlist list.append
#' @export
setMethod( f = "addAxe", signature = c("AmChart"),
           definition = function(.Object, axe = NULL, ...)
           {
             if( is.null(axe) && !missing(...) ){
               axe <- gaugeAxis(...)
             }else{}
             .Object@axes <- rlist::list.append(.Object@axes, listProperties(axe))
             validObject(.Object)
             return(.Object)
           }
)

# > @balloon: setters ####

#' @exportMethod setBalloon
setGeneric( name = "setBalloon",
            def = function(amChart, amBalloon = NULL, ...) {standardGeneric("setBalloon")} )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{amBalloon}: Object of class \code{\linkS4class{AmBalloon}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% setBalloon(adjustBorderColor = TRUE)
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{AmBalloon}} S4 class
#' @name setBalloon
#' @rdname setBalloon
#' @export
setMethod( f = "setBalloon", signature = c("AmChart"),
           definition = function(amChart, amBalloon = NULL, ...)
           {
             if( is.null(amBalloon) ){
               amBalloon <- amBalloon(...)
             }else{}
             amChart@balloon <- listProperties(amBalloon)
             validObject(amChart)
             return(amChart)
           }
)

# > @categoryAxis: setters ####

#' @exportMethod setCategoryAxis
setGeneric( name = "setCategoryAxis",
            def = function(amChart, categoryAxis = NULL , ...) {standardGeneric("setCategoryAxis")} )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{categoryAxis}: Object of class \code{\linkS4class{CategoryAxis}}.
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
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{CategoryAxis}} S4 class
#' @name setCategoryAxis
#' @rdname setCategoryAxis
#' @export
setMethod( f = "setCategoryAxis", signature = c("AmChart"),
           definition = function(amChart, categoryAxis = NULL, ...)
           {
             if( is.null(categoryAxis) ){
               categoryAxis <- categoryAxis(...)
             }else{}
             amChart@categoryAxis <- listProperties(categoryAxis)
             validObject(amChart)
             return(amChart)
           }
)

# > @categoryField: setters ####

#' @exportMethod setCategoryField
setGeneric( name = "setCategoryField",
            def = function(amChart, categoryField) {standardGeneric("setCategoryField")} )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @inheritParams amChart
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% setCategoryField("category")
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @name setCategoryField
#' @rdname setCategoryField
#' @export
setMethod( f = "setCategoryField", signature = c("AmChart", "character"),
           definition = function(amChart, categoryField)
           {
             amChart@categoryField <- categoryField
             validObject(amChart)
             return(amChart)
           }
)

# > @chartCursor : setters ####

#' @exportMethod setChartCursor
setGeneric(name = "setChartCursor",
           def = function(.Object, chartCursor = NULL, ...){ standardGeneric("setChartCursor") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{chartCursor}: Object of class \code{\linkS4class{ChartCursor}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% setChartCursor(chartCursor(oneBallOnly = TRUE))
#' 
#' # same result
#' amChart() %>>% setChartCursor(oneBallOnly = TRUE)
#' object <- amChart() %>>% setChartCursor()
#' chartCursor() %>>% class
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{ChartCursor}} S4 class
#' @name setChartCursor
#' @rdname setChartCursor
#' @export
setMethod( f = "setChartCursor", signature = c("AmChart"),
           definition = function(.Object, chartCursor = NULL, ...)
           {
             if( is.null(chartCursor) ){
               chartCursor <- chartCursor(...)
             }else {}
             .Object@chartCursor <- listProperties(chartCursor)
             validObject(.Object)
             return(.Object)
           }
)

# > @chartScrollbar : setters ####

#' @exportMethod setChartScrollbar
setGeneric(name = "setChartScrollbar",
           def = function(.Object, chartScrollbar = NULL, ...){ standardGeneric("setChartScrollbar") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{chartScrollbar}: Object of class \code{\linkS4class{ChartScrollbar}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% setChartScrollbar(chartScrollbar(oneBallOnly = TRUE))
#' amChart() %>>% setChartScrollbar()
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{ChartScrollbar}} S4 class
#' @name setChartScrollbar
#' @rdname setChartScrollbar
#' @export
setMethod( f = "setChartScrollbar", signature = c("AmChart"),
           definition = function(.Object, chartScrollbar = NULL, ...)
           {
             if ( is.null(chartScrollbar) ){
               chartScrollbar <- chartScrollbar( ... )
             }else{}
             .Object@chartScrollbar <- listProperties(chartScrollbar)
             validObject(.Object)
             return(.Object)
           }
)

# > @creditsPosition: setters ####

#' @exportMethod setCreditsPosition
setGeneric( name = "setCreditsPosition",
            def = function(amChart, creditsPosition) {standardGeneric("setCreditsPosition")} )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @inheritParams amChart
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% setCreditsPosition("top-right")
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @name setCreditsPosition
#' @rdname setCreditsPosition
#' @export
setMethod( f = "setCreditsPosition", signature = c("AmChart", "character"),
           definition = function(amChart, creditsPosition)
           {
             amChart@creditsPosition <- creditsPosition
             validObject(amChart)
             return(amChart)
           }
)

# > @dataProvider : setters ####

#' @title Setter for dataProvider
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{dataProvider}: Object of class \code{data.frame}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% setDataProvider(data.frame(key = c("FR", "US"), value = c(20,10)))
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @name setDataProvider
#' @rdname setDataProvider
#' @export
setMethod( f = "setDataProvider", signature = c("AmChart", "data.frame"),
           definition = function(.Object, dataProvider, keepNA = TRUE)
           {
             .Object@dataProvider <- toList(dataProvider, keepNA )
             validObject(.Object)
             return(.Object)
           }
)

# > @export: setters ####

#' @exportMethod setExport
setGeneric( name = "setExport",
            def = function(.Object, enabled = TRUE, ...) { standardGeneric("setExport") } )
#' @title Setter for export
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{...}: properties for export
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% setExport()
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @name setExport
#' @rdname setExport
#' @export
setMethod( f = "setExport", signature = c("AmChart"),
           definition = function(.Object, enabled = TRUE, ...)
           {
             .Object <- setProperties( .Object, export = list(enabled = enabled, ...) )
             validObject(.Object)
             return(.Object)
           }
)

# > @graphs : setters ####

#' @exportMethod setGraphs
setGeneric( name = "setGraphs",
            def = function(.Object, graphs) { standardGeneric("setGraphs") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{graphs}: Object of class \code{list}.
#' Each element must be an object of class \code{\linkS4class{AmGraph}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' graphs <- list(amGraph(balloonText = "balloonText"), amGraph(type = "column"))
#' amChart() %>>% setGraphs(graphs)
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{AmGraph}} S4 class
#' @name setGraphs
#' @rdname setGraphs
#' @export
setMethod( f = "setGraphs", signature = c("AmChart", "list"),
           definition = function(.Object, graphs)
           {
             rightClassElements <- prod(sapply(graphs, function(element) {is(element, "AmGraph")}))
             stopifnot(as.logical(rightClassElements))
             .Object@graphs <- lapply(graphs, listProperties)
             validObject(.Object)
             return(.Object)
           }
)

#' @exportMethod addGraph
setGeneric( name = "addGraph",
            def = function(.Object, amGraph = NULL, ...) { standardGeneric("addGraph") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{amGraph}: Object of class \code{\linkS4class{amGraph}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% addGraph(amGraph = amGraph(balloonText = "balloonText", "type" = "column"))
#' amChart() %>>% addGraph(balloonText = "balloonText", "type" = "column")
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{AmGraph}} S4 class
#' @name addGraph
#' @rdname addGraph
#' @importFrom rlist list.append
#' @export
setMethod( f = "addGraph", signature = c(.Object = "AmChart"),
           definition = function(.Object, amGraph = NULL , ...)
           {
             if( is.null(amGraph) && !missing(...)){
               amGraph <- amGraph(...)
             }else{}
             if( is(amGraph, "AmGraph") ){
               .Object@graphs <- rlist::list.append(.Object@graphs, listProperties(amGraph))
             }
             validObject(.Object)
             return(.Object)
           }
)

# > @graph: setters ####

#' @title Setter for graph
#' @details Method to use in case of AmChart of type \code{gantt}.
#' For other type see \code{\link{setGraphs}} or \code{\link{addGraph}}.
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}} and type \code{gantt}.
#' @param \code{graph}: Object of class \code{\linkS4class{AmGraph}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amGanttChart() %>>% setGraph()
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @name setGraph
#' @rdname setGraph
#' @export
setMethod( f = "setGraph", signature = c("AmChart"),
           definition = function(.Object, graph = NULL, ...)
           {
             if( is.null(graph) && !missing(...) ){
               graph <- amGraph(...)
             }else if( is.null(graph) && missing(...) ){
               graph <- amGraph(  balloonText = "[[value]]" )
             }else {}
             .Object@graph <- listProperties(graph)
             validObject(.Object)
             return(.Object)
           }
)

# > @guides : setters ####

#' @exportMethod setGuides
setGeneric( name = "setGuides",
            def = function(.Object, guides) { standardGeneric("setGuides") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{guides}: Object of class \code{list}.
#' Each element must be an object of class \code{\linkS4class{Guide}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' guides <- list(guide(fillAlpha = .1), guide(fillAlpha = .5))
#' amChart() %>>% setGuides(guides)
#' amChart(guides = guides)
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{Guide}} S4 class
#' @name setGuides
#' @rdname setGuides
#' @export
setMethod( f = "setGuides", signature = c("AmChart", "list"),
           definition = function(.Object, guides)
           {
             rightClassElements <- prod(sapply(guides, function(element) {is(element, "Guide")}))
             if(as.logical(rightClassElements))
               .Object@guides <- lapply(guides, listProperties)
             validObject(.Object)
             return(.Object)
           }
)

#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{guide}: Object of class \code{\linkS4class{Guide}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% addGuide(fillAlpha = .1)
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{Guide}} S4 class
#' @name addGuide
#' @rdname addGuide
#' @importFrom rlist list.append
#' @export
setMethod( f = "addGuide", signature = c("AmChart"),
           definition = function(.Object, guide = NULL, ...)
           {
             if( is.null(guide) && !missing(...) ){
               guide <- guide(...)
             }
             .Object@guides <- rlist::list.append(.Object@guides, listProperties(guide))
             validObject(.Object)
             return(.Object)
           }
)

# > @legend : setters ####

#' @exportMethod setLegend
setGeneric(name = "setLegend", def = function(.Object, amLegend = NULL, ...){ standardGeneric("setLegend") } )
#' @title Setter for Legend
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{amLegend}: Object of class \code{\linkS4class{AmLegend}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' # Without chaining
#' setLegend(amChart(), amLegend(useGraphSettings = TRUE))
#' setLegend(amChart(), useGraphSettings = TRUE)
#' 
#' # With chaining
#' amChart() %>>% setLegend(useGraphSettings = TRUE)
#' amChart() %>>% setLegend( amLegend(useGraphSettings = TRUE) )
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{AmLegend}} S4 class
#' @name setLegend
#' @rdname setLegend
#' @export
setMethod( f = "setLegend", signature = c("AmChart"),
           definition = function(.Object, amLegend = NULL, ...)
           {
             if( is.null(amLegend) && !missing(...) ){
               amLegend <- amLegend(...)
             }
             .Object@legend <- listProperties(amLegend)
             validObject(.Object)
             return(.Object)
           }
)

# > @listeners: setters ####

#' @exportMethod addListener
setGeneric(name = "addListener", def = function(.Object, name, expression) { standardGeneric("addListener") } )

#' @title Setter for Listener
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{name}: Object of class \code{character}.
#' @param \code{expression}: Object of class \code{character}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples 
#' library(pipeR)
#' amChart() %>>% addListener("select", "function onSelect (properties) {
#'      alert('selected nodes: ' + properties.nodes);}")
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{AmLegend}} S4 class
#' @name addListener
#' @rdname addListener
#' @export
setMethod( f = "addListener", signature = c("AmChart", "character", "character"),
           definition = function(.Object, name, expression)
           {
             .Object@listeners[[ eval(name) ]] <- JS(expression)
             # cat( class(JS(expression)), "\n")
             # cat( class( .Object@listeners[[ eval(name) ]] ), '\n' )
             validObject(.Object)
             return(.Object)
           }
)

# > @segments: setters ####

#' @exportMethod addSegment
setGeneric(name = "addSegment",
           def = function( .Object, categoryIDs, sgts ) { standardGeneric("addSegment") })

#' @title Add a segment to a category of AmChart
#' @details Use this methode in case of an AmChart.
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{categoryIDs}: Object of class \code{numeric}.
#' @param \code{sgts}: Object of class \code{data.frame}
#' ( or \code{list} of \code{data.frame} for multiple add ).
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amGanttChart( segmentsField = "segments", dataProvider = data.frame( category = c( "John", "Julia") ) ) %>>%
#' addSegment( 1, data.frame(start = 7, duration = 2:3, task = c("Task #1", "Task #2") ) ) %>>%
#' addSegment( 2, data.frame(start = 10, duration = 2:3, task = c("Task #1", "Task #2") ) )
#' 
#' ls <- list( data.frame(start = 7, duration = 2:3, task = c("Task #1", "Task #2") ), 
#' data.frame(start = 10, duration = 2:3, task = c("Task #1", "Task #2") ) )
#' amGanttChart( segmentsField = "segments", dataProvider = data.frame( category = c( "John", "Julia") ) ) %>>%
#' addSegment( 1:2,  ls)
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @name addSegment
#' @rdname addSegment
#' @export
setMethod(f = "addSegment", signature = c( .Object = "AmChart", categoryIDs = "numeric"),
          definition = function( .Object, categoryIDs, sgts ){
            
            if( prod( categoryIDs %in% 1:length(.Object@dataProvider) ) != 1 ){
              stop( "[addSegment]: range of argument categoryIDs must in [", 
                    1 , ":" , length(.Object@dataProvider), "]" )
            }else{}
            
            if( ! length( .Object@segmentsField ) > 0 ){
              stop( "[addSegment]: The property segmentsField must be non NULL" )
            }else{ segmentField <- as.character(.Object@segmentsField) }
            
            add <- function(.Object, categoryID, sgt){
              
              if( is(sgt, "data.frame") ){
                #cat("data.frame")
                .Object@dataProvider [[ eval(categoryID) ]] [ eval(segmentField) ] <<- list( toList( sgt ) )
              }else if ( is(sgt, "list") ){
                #cat("list")
                .Object@dataProvider [[ eval(categoryID) ]] [ eval(segmentField) ] <<- sgt
              }else{}
              # print( .Object@dataProvider [[ eval(categoryID) ]] [ eval(segmentField) ] )
              return( .Object )
            }
            
            if( length(categoryIDs) == 1 ){
              .Object <- add(.Object, categoryIDs, eval(sgts) )
            }else if( is( sgts, "list" ) ){
              invisible(
                sapply(categoryIDs, FUN = function(categoryID){
                  .Object <<- add(.Object, categoryID, eval(sgts) [[categoryID]])
                })
              )
            }else{}
            validObject(.Object)
            return(.Object)
          }
)

# > subData for drillChart: setters ####

#' @exportMethod addSubData
setGeneric(name = "addSubData", def = function(.Object, categoryIDs, data){standardGeneric("addSubData")})
#' @title Add subData for drilldrown
#' @description This method allows to add subdata for a chart with drilldown.
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{categoryIDs}: Object of class \code{numeric} (vector or simple value).
#' Indicates corresponding indice(s) of the \code{dataProvider} where to add the data.
#' @param \code{data}: Object of class \code{data.frame}. Data to draw at the second level,
#' after clicking on the serial / column.
#' @examples
#' library(pipeR)
#' amChart( dataProvider = data.frame(a = 1:5, b = 6:10) ) %>>%
#' addSubData( 3, data.frame(a = 1:10, b = 11:20) )
#' @family AmChart methods
#' @rdname addSubData
#' @name addSubData
#' @importFrom rlist list.append
#' @export
setMethod(f = "addSubData", signature = c("AmChart", "numeric", "data.frame"),
          definition = function(.Object, categoryIDs, data)
          {
            if( prod( categoryIDs %in% 1:length(.Object@dataProvider) ) != 1 ){
              stop( "[addSubData]: range of argument categoryIDs must in [", 
                    1 , ":" , length(.Object@dataProvider), "]" )
            }else{}
            
            add <- function(.Object, categoryID, data){
              if( is(data, "data.frame") ){
                #cat("data.frame")
                .Object@dataProvider [[ eval(categoryID) ]] <- rlist::list.append( .Object@dataProvider[[ eval(categoryID) ]],
                                                                                  subdata = toList(data) )
              }else if ( is(data, "list") ){
                #cat("list")
                .Object@dataProvider [[ eval(categoryID) ]] <- rlist::list.append( .Object@dataProvider[[ eval(categoryID) ]],
                                                                                   subdata = data )
              }else{}
              return( .Object )
            }
            
            if( length(categoryIDs) == 1 ){
              .Object <- add(.Object, categoryIDs, data )
            }else if( is( data, "list" ) ){
              invisible(
                sapply(categoryIDs, FUN = function(categoryID){
                  .Object <<- add(.Object, categoryID, data[[categoryID]])
                })
              )
            }else{}
            validObject(.Object)
            return(.Object)
          }
)

# > @subChartProperties (for drillChart): setters ####

#' @exportMethod setSubChartProperties
setGeneric(name = "setSubChartProperties",
           def = function(.Object, .subObject = NULL, ...){standardGeneric("setSubChartProperties")})
#' @title Add subData for drilldrown
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{.subObject}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{...}: Properties of AmChart.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amSerialChart() %>>% setSubChartProperties(type = "pie")
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @name setSubChartProperties
#' @rdname setSubChartProperties
#' @export
setMethod(f = "setSubChartProperties", signature = c("AmChart"),
          definition = function(.Object, .subObject = NULL, ...)
          {
            if ( is.null(.subObject) && !missing(...) ){
              .subObject <- amChart(...)
            }
            .Object@subChartProperties <- listProperties(.subObject)
            validObject(.Object)
            return(.Object)
          }
)

# > @titles : setters ####

#' @exportMethod setTitles
setGeneric( name = "setTitles",
            def = function(.Object, titles) { standardGeneric("setTitles") } )
#' @title Set a list of Title's
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{titles}: Object of class \code{list}.
#' Each element must be an object of class \code{\linkS4class{Titles}}.
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
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{Title}} S4 class
#' @name setTitles
#' @rdname setTitles
#' @export
setMethod( f = "setTitles", signature = c("AmChart", "list"),
           definition = function(.Object, titles)
           {
             rightClassElements <- prod(sapply(titles, function(element) {is(element, "Title")}))
             if( !rightClassElements ){
               stop("[setTitles]: each element of setTitles must be of class Title")
             }else{}
             .Object@titles <- lapply(titles, listProperties)
             validObject(.Object)
             return(.Object)
           }
)

#' @exportMethod addTitle
setGeneric( name = "addTitle",
            def = function(.Object, title = NULL, ...) { standardGeneric("addTitle") } )
#' @title Add a Title
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @inheritParams amChart
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% addTitle( text = "balloonText" )
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{Title}} S4 class
#' @name addTitle
#' @rdname addTitle
#' @importFrom rlist list.append
#' @export
setMethod( f = "addTitle", signature = c("AmChart"),
           definition = function(.Object, title = NULL, ...)
           {
             if ( is.null(title) && !missing(...) ){
               title <- title(...)
             }
             .Object@titles <- rlist::list.append(.Object@titles, listProperties(title))
             validObject(.Object)
             return(.Object)
           }
)

# > @trendLines : setters ####

#' @exportMethod setTrendLines
setGeneric( name = "setTrendLines",
            def = function(.Object, trendLines) { standardGeneric("setTrendLines") } )
#' @title Setter for trendLines
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{trendLines}: Object of class \code{list}.
#' Each element must be of class \code{\linkS4class{TrendLine}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' trendLines <- list(trendLine(initialValue = 1, finalValue = 5), trendLine(initialValue = 7, finalValue = 19))
#' amChart() %>>% setTrendLines(trendLines)
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{TrendLine}} S4 class
#' @name setTrendLines
#' @rdname setTrendLines
#' @export
setMethod( f = "setTrendLines", signature = c("AmChart", "list"),
           definition = function(.Object, trendLines)
           {
             rightClassElements <- prod(sapply(trendLines, function(element){class(element) == "TrendLine"}))
             stopifnot(as.logical(rightClassElements))
             .Object@trendLines <- lapply(trendLines, listProperties)
             validObject(.Object)
             return(.Object)
           }
)

#' @exportMethod addTrendLine
setGeneric( name = "addTrendLine",
            def = function(.Object, trendLine = NULL, ...) { standardGeneric("addTrendLine") } )
#' @title Add a trendLine
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @inheritParams amChart
#' @examples
#' library(pipeR)
#' amChart() %>>% addTrendLine( initialValue = 1, finalValue = 11 )
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{TrendLine}} S4 class
#' @name addTrendLine
#' @rdname addTrendLine
#' @export
setMethod( f = "addTrendLine", signature = c("AmChart"),
           definition = function(.Object, trendLine = NULL, ...)
           {
             if( is.null(trendLine) && !missing(...) ){
               trendLine <- trendLine(...)
             }
             .Object@trendLines <- list.append(.Object@trendLines, listProperties(trendLine))
             validObject(.Object)
             return(.Object)
           }
)

# > @type : setters ####

#' @title Setter for Type
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{type}: Object of class \code{\linkS4class{character}}.
#' @inheritParams amChart
#' @examples
#' library(pipeR)
#' amChart() %>>% setType("pie")
#' @name setType
#' @rdname setType
#' @export
setMethod( f = "setType", signature = c("AmChart", "character"),
           definition = function(.Object, type)
           {
             .Object@type <- type
             validObject(.Object)
             return(.Object)
           }
)


# > @valueAxes : setters ####

#' @exportMethod setValueAxes
setGeneric(name = "setValueAxes",
           def = function(.Object, valueAxes = NULL, ...){ standardGeneric("setValueAxes") } )
#' @title Setter for ValueAxes
#' @description Add a list of ValueAxes objects to an Amchart.
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{valueAxes}: Object of class \code{list}.
#' Each element must be of class \code{\linkS4class{ValueAxes}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' valueAxes <- list(valueAxis(axisTitleOffset = 12, tickLength = 10), valueAxis(axisTitleOffset = 10, tickLength = 10))
#' object <- amChart() %>>% setValueAxes(valueAxes)
#' \dontrun{
#' lapply(valueAxes, listProperties)
#' amChart(valueAxes = valueAxes)
#' }
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{ValueAxes}} S4 class
#' @name setValueAxes
#' @rdname setValueAxes
#' @export
setMethod( f = "setValueAxes", signature = c("AmChart", "list"),
           definition = function(.Object, valueAxes)
           {
             rightClassElements <- prod(sapply(valueAxes, function(element) {is(element, "ValueAxis")}))
             stopifnot(as.logical(rightClassElements))
             .Object@valueAxes <- lapply(valueAxes, listProperties)
             validObject(.Object)
             return(.Object)
           }
)

#' @exportMethod addValueAxes
setGeneric( name = "addValueAxes",
            def = function(.Object, valueAxis = NULL, ... ) { standardGeneric("addValueAxes") } )
#' @title Add a ValueAxes
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{valueAxis}: Object of class \code{\linkS4class{ValuesAxis}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amChart() %>>% addValueAxes( axisTitleOffset = 12, tickLength = 10 )
#' @family AmChart setters
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{ValueAxis}} S4 class
#' @name addValueAxes
#' @rdname addValueAxes
#' @importFrom rlist list.append
#' @export
setMethod( f = "addValueAxes", signature = c("AmChart"),
           definition = function(.Object, valueAxis = NULL, ...)
           {
             if( is.null(valueAxis) && !missing(...) ){
               valueAxis <- valueAxis(...)
             }
             .Object@valueAxes <- rlist::list.append(.Object@valueAxes, listProperties(valueAxis))
             validObject(.Object)
             return(.Object)
           }
)

# > @valueAxis: setters ####

#' @title Setter for valueAxis
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}
#' with \code{type = "gantt"}.
#' @param \code{valueAxis}: Object of class \code{\linkS4class{ValueAxes}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' library(pipeR)
#' amGanttChart() %>>% setValueAxis()
#' @family AmChart setters
#' @family AmChart methods
#' @name setValueAxis
#' @rdname setValueAxis
#' @export
setMethod( f = "setValueAxis", signature = c("AmChart"),
           definition = function(.Object, valueAxis = NULL, ...)
           {
             if( is.null(valueAxis) && !missing(...) ){
               valueAxis <- valueAxis(...)
             }else if( is.null(valueAxis) && missing(...) ){
               valueAxis <- valueAxis(  autoGridCount = TRUE )
             }else {}
             .Object@valueAxis <- listProperties(valueAxis)
             validObject(.Object)
             return(.Object)
           }
)
