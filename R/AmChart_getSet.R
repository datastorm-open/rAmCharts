#' @include sharedGenerics.R CategoryAxis.R AmGraph.R ValueAxis.R ChartCursor.R ChartScrollbar.R AmLegend.R TrendLine.R Title.R Label.R GaugeArrow.R Guide.R
NULL

# > @allLabels : setters ####

#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setAllLabels",
           def = function(.Object, allLabels) {standardGeneric("setAllLabels")})
#' @examples
#' allLabels <- list(label(text = "balloonText"), label(text = "column"))
#' print(setAllLabels(.Object = amSerialChart(), allLabels = allLabels))
#' # equivalent to:
#' print(amSerialChart(allLabels = allLabels))
#' # ---
#' @rdname initialize-AmChart
setMethod(f = "setAllLabels", signature = c("AmChart", "list"),
          definition = function(.Object, allLabels)
          {
            rightClassElements <- prod(sapply(allLabels, function(element) {is(element, "Label")}))
            if (!rightClassElements) {
              stop("[setAllLabels]: each element of allLabels must be of class Label")
            } else {}
            .Object@allLabels <- lapply(allLabels, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @param label (optional) \linkS4class{Label}.
#' Argument of method \code{addLabel}.
#' @examples
#' print(addLabel(.Object = amSerialChart(), text = "balloonText"))
#' # equivalent to:
#' label_obj <- label(text = "balloonText")
#' print(addLabel(.Object = amSerialChart(), label = label_obj))
#' \dontrun{
#' # Error use cases:
#'  addLabel(.Object = amChart())
#'  addLabel(.Object = amChart(), label = "another class")
#' }
#' # ---
#' @seealso \linkS4class{Label}
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addLabel",
            def = function(.Object, label = NULL, ...) {standardGeneric("addLabel")})
setClassUnion(name = "LabelOrMissing", members = c("Label", "missing"))
#' @rdname initialize-AmChart
setMethod(f = "addLabel", signature = c("AmChart", "LabelOrMissing"),
          definition = function(.Object, label = NULL, ...)
          {
            if (is.null(label) && !missing(...)) {
              label <- label(...)
            } else if (is.null(label) && missing(...)) {
              stop("You must either provide argument label or give its properties")
            } else {}
            
            .Object@allLabels <- rlist::list.append(.Object@allLabels, listProperties(label))
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' arrows_ls <- list(gaugeArrow(value = 130), gaugeArrow(value = 150) )
#' print(setArrows(.Object = amAngularGaugeChart(), arrows = arrows_ls))
#' # equivalent to:
#' print(amAngularGaugeChart(arrows = arrows_ls))
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setArrows",
            def = function(.Object, arrows = NULL) { standardGeneric("setArrows") } )
#' @rdname initialize-AmChart
setMethod(f = "setArrows", signature = c("AmChart"),
          definition = function(.Object, arrows = NULL)
          {
            rightClassElements <- prod(sapply(arrows, function(element) {is(element, "GaugeArrow")}))
            if (!rightClassElements) {
              stop("[setArrows]: each element of arrows must be of class GaugeArrow")
            } else {}
            .Object@arrows <- lapply(arrows, listProperties)
            
            validObject(.Object)
            return(.Object)
          })

#' @param arrow (optional) \linkS4class{GaugeArrow}.
#' Argument of method \code{addArrow}.
#' @seealso \linkS4class{GaugeArrow}.
#' @examples
#' print(addArrow(.Object = amAngularGaugeChart(), value = 10))
#' # equivalent to:
#' gaugeArrow_obj <- gaugeArrow(value = 10)
#' print(addArrow(.Object = amAngularGaugeChart(), arrow = gaugeArrow_obj))
#' \dontrun{
#' # Error use cases: 
#' addArrow(.Object = amAngularGaugeChart())
#' addArrow(.Object = amAngularGaugeChart(), arrow = "error")
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addArrow",
            def = function(.Object, arrow = NULL, ...) { standardGeneric("addArrow") } )
setClassUnion(name = "GaugeArrowOrMissing", members = c("GaugeArrow", "missing"))
#' @rdname initialize-AmChart
setMethod(f = "addArrow", signature = c("AmChart", "GaugeArrowOrMissing"),
           definition = function(.Object, arrow = NULL, ...)
           {
             if (is.null(arrow) && !missing(...)) {
               arrow <- gaugeArrow(...)
             } else if (is.null(arrow) && missing(...)) {
               stop("You must either provide argument arrow or give its properties")
             } else {}
             
             .Object@arrows <- rlist::list.append(.Object@arrows, listProperties(arrow))
             validObject(.Object)
             return(.Object)
           })

#' @examples
#' axes_ls <- list(gaugeAxis(value = 130), gaugeAxis(value = 150))
#' setAxes(.Object = amAngularGaugeChart(), axes = axes_ls)
#' # equivalent to:
#' amChart(axes = axes_ls)
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setAxes",
            def = function(.Object, axes, ...) {standardGeneric("setAxes")})
#' @rdname initialize-AmChart
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

#' @param axe (optional) \linkS4class{GaugeAxis}.
#' Argument of method \code{addAxe}.
#' @examples
#' print(addAxe(.Object = amAngularGaugeChart(), startValue = 0, enValue = 100, valueInterval = 10))
#' # equivalent to:
#' gaugeAxis_obj <- gaugeAxis(startValue = 0, enValue = 100, valueInterval = 10)
#' print(addAxe(.Object = amAngularGaugeChart(), axe = gaugeAxis_obj))
#' \dontrun{
#' # Error use cases: 
#' addAxe(.Object = amAngularGaugeChart())
#' addAxe(.Object = amAngularGaugeChart(), axe = "error")
#' }
#' # ---
#' @seealso \linkS4class{GaugeAxis}
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addAxe",
            def = function(.Object, axe = NULL, ...) {standardGeneric("addAxe")})
setClassUnion(name = "GaugeAxisOrMissing", members = c("GaugeAxis", "missing"))
#' @rdname initialize-AmChart
setMethod(f = "addAxe", signature = c("AmChart", "GaugeAxisOrMissing"),
          definition = function(.Object, axe = NULL, ...)
          {
            if (is.null(axe) && !missing(...)) {
              axe <- gaugeAxis(...)
            } else if (is.null(axe) && missing(...)) {
              stop("You must either give 'axe' argement or its properties")
            } else {}
            
            .Object@axes <- rlist::list.append(.Object@axes, listProperties(axe))
            validObject(.Object)
            return(.Object)
          })

#' @param amBalloon \linkS4class{AmBalloon}, argument of method 'setBalloon'.
#' @examples
#' print(setBalloon(.Object = amSerialChart(), adjustBorderColor = TRUE, fillColor = "#FFFFFF",
#'                  color = "#000000", cornerRadius = 5))
#' # equivalent to:
#' amBalloon_obj <- amBalloon(adjustBorderColor = TRUE, fillColor = "#FFFFFF",
#'                            color = "#000000", cornerRadius = 5)
#' print(setBalloon(.Object = amSerialChart(), amBalloon = amBalloon_obj))
#' \dontrun{
#' # Error use cases: 
#' setBalloon(.Object = amSerialChart())
#' setBalloon(.Object = amSerialChart(), amBalloon = "error")
#' }
#' # ---
#' @seealso \linkS4class{AmBalloon}
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setBalloon",
            def = function(.Object, amBalloon = NULL, ...) {standardGeneric("setBalloon")})
setClassUnion(name = "AmBalloonOrMissing", members = c("AmBalloon", "missing"))
#' @rdname initialize-AmChart
setMethod(f = "setBalloon", signature = c("AmChart", "AmBalloonOrMissing"),
           definition = function(.Object, amBalloon = NULL, ...)
           {
             if (is.null(amBalloon) && !missing(...)) {
               amBalloon <- amBalloon(...)
             } else if (is.null(amBalloon) && missing(...)) {
               stop("You must either give argument 'amBalloon' or its properties")
             } else {}
             
             .Object@balloon <- listProperties(amBalloon)
             validObject(.Object)
             return(.Object)
           })

#' @examples
#' print(setCategoryAxis(.Object = amSerialChart(), gridPosition = "start"))
#' # equivalent to:
#' categoryAxis_obj <- categoryAxis(gridPosition = "start")
#' print(setCategoryAxis(.Object = amSerialChart(), categoryAxis = categoryAxis_obj))
#' \dontrun{
#' # The argument categoryAxis must be an object of class CategoryAxis
#' setCategoryAxis(.Object = amSerialChart(), categoryAxis = "error")
#' }
#' # ---
#' @seealso \linkS4class{CategoryAxis}
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setCategoryAxis",
            def = function(.Object, categoryAxis = NULL , ...) {standardGeneric("setCategoryAxis")} )
#' @rdname initialize-AmChart
setMethod(f = "setCategoryAxis", signature = c("AmChart"),
          definition = function(.Object, categoryAxis = NULL, ...)
          {
            if (is.null(categoryAxis) && !missing(...)) {
              categoryAxis <- categoryAxis(...)
            } else if (is.null(categoryAxis) && missing(...)) {
              stop("You must either give argument 'categoryAxis' or its properties")
            } else {}
            
            .Object@categoryAxis <- listProperties(categoryAxis)
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' print(setCategoryField(.Object = amSerialChart(), categoryField = "country"))
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setCategoryField",
            def = function(.Object, categoryField) {standardGeneric("setCategoryField")} )
#' @rdname initialize-AmChart
setMethod(f = "setCategoryField", signature = c("AmChart", "character"),
          definition = function(.Object, categoryField)
          {
            .Object@categoryField <- categoryField
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' # with default value, nor argument needed
#' print(setChartCursor(.Object = amSerialChart()))
#' # other example
#' print(setChartCursor(.Object = amSerialChart(), oneBallOnly = TRUE))
#' # equivalent to
#' chartCursor_obj <- chartCursor(oneBallOnly = TRUE)
#' print(setChartCursor(.Object = amSerialChart(), chartCursor = chartCursor_obj))
#' \dontrun{
#' Error use case:
#' setChartCursor(.Object = amSerialChart(), chartCursor = "error")
#' }
#' # ---
#' @seealso \linkS4class{ChartCursor}
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setChartCursor",
           def = function(.Object, chartCursor = NULL, ...) {standardGeneric("setChartCursor")})
setClassUnion("ChartCursorOrMissing", c("ChartCursor", "missing"))
#' @rdname initialize-AmChart
setMethod(f = "setChartCursor", signature = c("AmChart", "ChartCursorOrMissing"),
          definition = function(.Object, chartCursor = NULL, ...)
          {
            if (is.null(chartCursor) && !missing(...)) {
              chartCursor <- chartCursor(...)
            } else if (is.null(chartCursor) && missing(...)) {
              chartCursor <- chartCursor()
              message("default 'chartCursor' added")
            } else {}
            
            .Object@chartCursor <- listProperties(chartCursor)
            validObject(.Object)
            return(.Object)
          })

# > @chartScrollbar : setters ####

#' @examples
#' # Add the default scrollbar
#' print(setChartScrollbar(.Object = amSerialChart()))
#' # equivalent to:
#' chartScrollbar_obj <- chartScrollbar(updateOnReleaseOnly = FALSE)
#' print(setChartScrollbar(.Object = amSerialChart(), chartScrollbar = chartScrollbar_obj))
#' \dontrun{
#' setChartScrollbar(.Object = amSerialChart(), chartScrollbar = list(updateOnReleaseOnly = FALSE))
#' }
#' # ---
#' @seealso \linkS4class{ChartScrollbar}
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setChartScrollbar",
           def = function(.Object, chartScrollbar = NULL, ...) {standardGeneric("setChartScrollbar")})
setClassUnion("ChartScrollbarOrMissing", c("ChartScrollbar", "missing"))
#' @rdname initialize-AmChart
setMethod(f = "setChartScrollbar", signature = c("AmChart", "ChartScrollbarOrMissing"),
          definition = function(.Object, chartScrollbar = NULL, ...)
          {
            if (is.null(chartScrollbar) && !missing(...)) {
              chartScrollbar <- chartScrollbar(...)
            } else if (is.null(chartScrollbar) && missing(...)) {
              chartScrollbar <- chartScrollbar()
              message("default 'chartScrollbar' added")
            } else {}
            
            .Object@chartScrollbar <- listProperties(chartScrollbar)
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' print(setCreditsPosition(.Object = amPieChart(), creditsPosition = "top-right"))
#' \dontrun{
#' Error use case:
#' setCreditsPosition(.Object = amPieChart(), creditsPosition = "top-center")
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setCreditsPosition",
            def = function(.Object, creditsPosition) {standardGeneric("setCreditsPosition")} )
#' @rdname initialize-AmChart
setMethod(f = "setCreditsPosition", signature = c("AmChart", "character"),
          definition = function(.Object, creditsPosition)
          {
            .Object@creditsPosition <- creditsPosition
            validObject(.Object)
            return(.Object)
          })

setClassUnion("logicalOrMissing", c("logical", "missing"))
#' @param keepNA
#' object of class \code{logical}, default \code{TRUE}.
#' Indicates if \code{NULL} values have to be kept or ignored. 
#' @examples
#' dataProvider_obj <- data.frame(key = c("FR", "US", "GER", "ENG", "IT" ),
#'                                value = round(runif(5, max = 100)))
#' print(setDataProvider(.Object = amPieChart(), dataProvider = dataProvider_obj))
#' # ---
#' @rdname initialize-AmChart
#' @export
setMethod(f = "setDataProvider", signature = c("AmChart", "data.frame", "logicalOrMissing"),
           definition = function(.Object, dataProvider, keepNA = TRUE)
           {
             .Object@dataProvider <- toList(dataProvider, keepNA )
             validObject(.Object)
             return(.Object)
           })


#' @param enabled \code{logical}, default \code{TRUE}.
#' Should the export button be shown ?
#' @examples
#' print(setExport(.Object = amSerialChart()))
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setExport",
            def = function(.Object, enabled = TRUE, ...) { standardGeneric("setExport") } )
#' @rdname initialize-AmChart
setMethod(f = "setExport", signature = c("AmChart"),
           definition = function(.Object, enabled = TRUE, ...)
           {
             .Object <- setProperties( .Object, export = list(enabled = enabled, ...) )
             validObject(.Object)
             return(.Object)
           })

# > @graphs : setters ####

#' @examples
#' graphs_ls <- list(amGraph(balloonText = "balloonText"), amGraph(type = "column"))
#' print(setGraphs(.Object = amChart(), graphs = graphs_ls))
#' \dontrun{
#' graphs_ls <- list(list(balloonText = "balloonText"), list(type = "column"))
#' setGraphs(.Object = amChart(), graphs = graphs_ls)
#' }
#' # ---
#' @seealso \linkS4class{AmGraph}
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setGraphs",
            def = function(.Object, graphs) { standardGeneric("setGraphs") } )
#' @rdname initialize-AmChart
setMethod(f = "setGraphs", signature = c("AmChart", "list"),
          definition = function(.Object, graphs)
          {
            rightClassElements <- prod(sapply(graphs, function(element) {is(element, "AmGraph")}))
            stopifnot(as.logical(rightClassElements))
            .Object@graphs <- lapply(graphs, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @param amGraph (optional) \linkS4class{AmGraph}.
#' @examples
#' print(addGraph(.Object = amSerialChart(), balloonText = "balloonText", "type" = "column"))
#' # equivalent to
#' amGraph_obj <- amGraph(balloonText = "balloonText", "type" = "column")
#' print(addGraph(.Object = amSerialChart(), amGraph = amGraph_obj))
#' # ---
#' @seealso \linkS4class{AmGraph} S4 class
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addGraph",
            def = function(.Object, amGraph = NULL, ...) {standardGeneric("addGraph")})
setClassUnion(name = "AmGraphOrMissing", members = c("AmGraph", "missing"))
#' @rdname initialize-AmChart
setMethod(f = "addGraph", signature = c("AmChart", "AmGraphOrMissing"),
          definition = function(.Object, amGraph = NULL , ...)
          {
            if (is.null(amGraph) && !missing(...)) {
              amGraph <- amGraph(...)
            } else if (is.null(amGraph) && missing(...)) {
              stop("You must either give arguemnt 'amGraph' or its properties")
            } else {}
            
            .Object@graphs <- rlist::list.append(.Object@graphs, listProperties(amGraph))
            validObject(.Object)
            return(.Object)
          })

#' @details Method \code{setGraph} is only valid for Gantt Charts.
#' @examples
#' print(setGraph(.Object = amGanttChart(), id = "amGraph-1"))
#' # equivalent to
#' amGraph_obj <- amGraph(id = "amGraph-1")
#' print(setGraph(.Object = amGanttChart(), amGraph = amGraph_obj))
#' # ---
#' @seealso \linkS4class{AmChart}
#' @rdname initialize-AmChart
#' @export
setMethod(f = "setGraph", signature = c("AmChart", "AmGraphOrMissing"),
          definition = function(.Object, graph = NULL, ...)
          {
            if (is.null(graph) && !missing(...)) {
              graph <- amGraph(...)
            } else if (is.null(graph) && missing(...)) {
              stop("You must either give arguemnt 'amGraph' or its properties")
            } else {}
            
            .Object@graph <- listProperties(graph)
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' guides_ls <- list(guide(fillAlpha = .1), guide(fillAlpha = .5))
#' print(setGuides(.Object = amSerialChart(), guides = guides_ls))
#' # or...
#' amSerialChart(guides = guides_ls)
#' # ---
#' @seealso \linkS4class{Guide}
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setGuides",
           def = function(.Object, guides) {standardGeneric("setGuides")})
#' @rdname initialize-AmChart
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

setClassUnion(name = "GuideOrMissing", members = c("Guide", "missing"))
#' @param guide (optional) \linkS4class{Guide}.
#' @examples
#' print(addGuide(.Object = amSerialChart(), fillAlpha = .1, value = 0, toVAlue = 10))
#' # equivalent to
#' guide_obj <- guide(fillAlpha = .1, value = 0, toVAlue = 10)
#' print(addGuide(.Object = amSerialChart(), guide = guide_obj))
#' # ---
#' @seealso \linkS4class{Guide}
#' @rdname initialize-AmChart
#' @export
setMethod(f = "addGuide", signature = c("AmChart", "GuideOrMissing"),
          definition = function(.Object, guide = NULL, ...)
          {
            if (is.null(guide) && !missing(...)) {
              guide <- guide(...)
            } else if (is.null(guide) && missing(...)) {
              stop("You must provide either argument 'guide' or its properties")
            } else {}
            
            .Object@guides <- rlist::list.append(.Object@guides, listProperties(guide))
            validObject(.Object)
            return(.Object)
          })

#' @param amLegend (optional) \linkS4class{AmLegend}.
#' @examples
#' print(setLegend(.Object = amChart(), amLegend = amLegend(useGraphSettings = TRUE)))
#' # equivalent to:
#' print(setLegend(.Object = amChart(), useGraphSettings = TRUE))
#' # ---
#' @seealso \linkS4class{AmLegend} S4 class
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setLegend",
           def = function(.Object, amLegend = NULL, ...) {standardGeneric("setLegend")})
setClassUnion(name = "AmLegendOrMissing", members = c("AmLegend", "missing"))
#' @rdname initialize-AmChart
setMethod(f = "setLegend", signature = c("AmChart", "AmLegendOrMissing"),
          definition = function(.Object, amLegend = NULL, ...)
          {
            if (is.null(amLegend) && !missing(...)) {
              amLegend <- amLegend(...)
            } else if (is.null(amLegend) && missing(...)) {
              stop("You must provide either argument 'amLegend' or its properties")
            } else {}
            
            .Object@legend <- listProperties(amLegend)
            validObject(.Object)
            return(.Object)
          })

#' @param categoryIDs \code{numeric}, see details.
#' @param sgts \code{data.frame}
#' ( or \code{list} of \code{data.frame} for multiple add ).
#' @examples
#' pipeR::pipeline(
#'   amGanttChart(segmentsField = "segments"),
#'   setDataProvider(data.frame(category = c( "John", "Julia"))),
#'   addSegment(1, data.frame(start = 7, duration = 2:3, task = c("Task #1", "Task #2"))),
#'   addSegment(2, data.frame(start = 10, duration = 2:3, task = c("Task #1", "Task #2")))
#' )
#' # ---
#' ls <- list(data.frame(start = 7, duration = 2:3, task = c("Task #1", "Task #2")), 
#'            data.frame(start = 10, duration = 2:3, task = c("Task #1", "Task #2")))
#' pipeR::pipeline(
#'   amGanttChart(segmentsField = "segments"),
#'   setDataProvider(data.frame(category = c( "John", "Julia"))),
#'   addSegment(1:2,  ls)
#' )
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addSegment",
           def = function (.Object, categoryIDs, sgts) { standardGeneric("addSegment") })
#' @rdname initialize-AmChart
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
#' @details 'addSubData' allows to add subdata for a chart with drilldown. 
#' In this case, categoryIDs indicates corresponding indice(s)
#' of the \code{dataProvider} where to add the data.
#' @param data \code{data.frame}. Data to draw at the second level,
#' after clicking on the column.
#' @examples
#' amChart_obj <- amChart(dataProvider = data.frame(a = 1:5, b = 6:10))
#' print(addSubData(.Object = amChart_obj, categoryIDs = 3, data = data.frame(a = 1:10, b = 11:20)))
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addSubData",
           def = function(.Object, categoryIDs, data) {standardGeneric("addSubData")})
#' @rdname initialize-AmChart
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

#' @param .subObject \linkS4class{AmChart}.
#' @examples
#' print(setSubChartProperties(.Object = amSerialChart(), type = "serial"))
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setSubChartProperties",
           def = function(.Object, .subObject = NULL, ...) {standardGeneric("setSubChartProperties")})
#' @rdname initialize-AmChart
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

#' @examples
#' titles_ls <- list(title(text = "balloonText"), title(text = "column"))
#' print(setTitles(.Object = amXYChart(), titles = titles_ls))
#' # or...
#' print(amChart(titles = titles_ls))
#' # ---
#' @seealso \linkS4class{Title}
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setTitles",
            def = function(.Object, titles) { standardGeneric("setTitles") } )
#' @rdname initialize-AmChart
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

#' @param title (optional) \linkS4class{Title}, argument of method \code{addTitle}.
#' @examples
#' print(addTitle(.Object = amPieChart(), text = "balloonText", size = 15))
#' # equivalent to
#' title_obj <- title(text = "balloonText", size = 15)
#' print(addTitle(.Object = amPieChart(), title = title_obj))
#' # ---
#' @seealso \linkS4class{Title}
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addTitle",
            def = function(.Object, title = NULL, ...) {standardGeneric("addTitle")})
setClassUnion(name = "TitleOrMissing", members = c("Title", "missing"))
#' @rdname initialize-AmChart
setMethod(f = "addTitle", signature = c("AmChart", "TitleOrMissing"),
          definition = function(.Object, title = NULL, ...)
          {
            if (is.null(title) && !missing(...)) {
              title <- title(...)
            } else if (is.null(title) && missing(...)) {
              stop("You must either give argument 'title' or its properties")
            } else {}
            
            .Object@titles <- rlist::list.append(.Object@titles, listProperties(title))
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' trendLines <- list(trendLine(initialValue = 1, finalValue = 5),
#'                    trendLine(initialValue = 7, finalValue = 19))
#' print(setTrendLines(.Object = amChart(), trendLines = trendLines))
#'# or... 
#' print(amChart(trendLines = trendLines)) # Equivalent
#' @seealso \linkS4class{TrendLine}
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setTrendLines",
            def = function(.Object, trendLines) {standardGeneric("setTrendLines")})
#' @rdname initialize-AmChart
setMethod(f = "setTrendLines", signature = c("AmChart", "list"),
          definition = function(.Object, trendLines)
          {
            rightClassElements <- prod(sapply(trendLines, function(element) {class(element) == "TrendLine"}))
            stopifnot(as.logical(rightClassElements))
            .Object@trendLines <- lapply(trendLines, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @param trendLine (optional) \linkS4class{TrendLine}.
#' Argument of method \code{addTrendLine}.
#' @examples 
#' addTrendLine(.Object = amSerialChart(), initialValue = 1, initialXValue = 1,
#'              finalValue = 11, finalXValue = 12)
#' # equivalent to:
#' trendLine_obj <- trendLine(initialValue = 1, initialXValue = 1, finalValue = 11, finalXValue = 12)
#' addTrendLine(.Object = amSerialChart(), trendLine = trendLine_obj)
#' @seealso \linkS4class{TrendLine}
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addTrendLine",
           def = function(.Object, trendLine = NULL, ...) {standardGeneric("addTrendLine")})
setClassUnion("TrendLineOrMissing", c("TrendLine", "missing"))
#' @rdname initialize-AmChart
setMethod(f = "addTrendLine", signature = c("AmChart", "TrendLineOrMissing"),
          definition = function(.Object, trendLine = NULL, ...)
          {
            if (is.null(trendLine) && !missing(...)) {
              trendLine <- trendLine(...)
            } else if (is.null(trendLine) && missing(...)) {
              stop("You must provide either argument 'trendline' or its properties")
            } else {}
            
            .Object@trendLines <- rlist::list.append(.Object@trendLines, listProperties(trendLine))
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' setType(.Object = amChart(), type = "pie")
#' # equivalent to:
#' amPieChart()
#' @rdname initialize-AmChart
#' @export
setMethod(f = "setType", signature = c("AmChart", "character"),
          definition = function(.Object, type)
          {
            .Object@type <- type
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' valueAxes <- list(valueAxis(axisTitleOffset = 12, tickLength = 10),
#'                   valueAxis(axisTitleOffset = 10, tickLength = 10))
#' setValueAxes(.Object = amSerialChart(), valueAxes = valueAxes)
#' # or...
#' amSerialChart(valueAxes = valueAxes)
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setValueAxes",
           def = function(.Object, valueAxes) {standardGeneric("setValueAxes")})
#' @rdname initialize-AmChart
setMethod(f = "setValueAxes", signature = c("AmChart", "list"),
          definition = function(.Object, valueAxes)
          {
            rightClassElements <- prod(sapply(valueAxes, function(element) {is(element, "ValueAxis")}))
            stopifnot(as.logical(rightClassElements))
            .Object@valueAxes <- lapply(valueAxes, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @details For method \code{addValueAxes}: valueAxis is optional.
#' @examples
#' addValueAxes(.Object = amSerialChart(), axisTitleOffset = 12, tickLength = 10)
#' # equivalent to:
#' valueAxis_obj <- valueAxis(axisTitleOffset = 12, tickLength = 10)
#' addValueAxes(.Object = amSerialChart(), valueAxis = valueAxis_obj)
#' # ---
#' @seealso \linkS4class{ValueAxis}
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addValueAxes",
            def = function(.Object, valueAxis = NULL, ... ) {standardGeneric("addValueAxes")})
setClassUnion("ValueAxisOrMissing", c("ValueAxis", "missing"))
#' @rdname initialize-AmChart
setMethod(f = "addValueAxes", signature = c("AmChart", "ValueAxisOrMissing"),
          definition = function(.Object, valueAxis = NULL, ...)
          {
            if (is.null(valueAxis) && !missing(...)) {
              valueAxis <- valueAxis(...)
            } else if (is.null(valueAxis) && missing(...)) {
              stop("You must provide argument 'valueAxis' or its properties")
            } else {}
            
            .Object@valueAxes <- rlist::list.append(.Object@valueAxes, listProperties(valueAxis))
            validObject(.Object)
            return(.Object)
          })

#' @details Method \code{setValueAxis} is only valid for Gantt charts.
#' @examples
#' print(setValueAxis(.Object = amGanttChart()))
#' print(setValueAxis(.Object = amGanttChart(), type = "date"))
#' @rdname initialize-AmChart
#' @export
setMethod(f = "setValueAxis", signature = c("AmChart", "ValueAxisOrMissing"),
          definition = function(.Object, valueAxis = NULL, ...)
          {
            if (is.null(valueAxis) && !missing(...)) {
              valueAxis <- valueAxis(...)
            } else if (is.null(valueAxis) && missing(...)) {
              valueAxis <- valueAxis(autoGridCount = TRUE)
            } else {}
            .Object@valueAxis <- listProperties(valueAxis)
            validObject(.Object)
            return(.Object)
          })
