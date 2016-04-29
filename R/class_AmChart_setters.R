#' @include classUnions.R utils_sharedGenerics.R class_CategoryAxis.R
NULL

#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setAllLabels", def = function(.Object, allLabels) {standardGeneric("setAllLabels")})
#' @examples
#' \donttest{
#' allLabels <- list(label(text = "balloonText"), label(text = "column"))
#' amSerialChart(allLabels = allLabels)
#' }
#' # ---
#' @rdname initialize-AmChart
setMethod(f = "setAllLabels", signature = c("AmChart", "list"),
          definition = function(.Object, allLabels)
          {
            rightClassElements <- all(sapply(allLabels, function(element) {is(element, "Label")}))
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
#' \donttest{
#' addLabel(.Object = amSerialChart(), text = "balloonText")
#' # equivalent to:
#' label_obj <- label(text = "balloonText")
#' addLabel(.Object = amSerialChart(), label = label_obj)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addLabel", def = function(.Object, label = NULL, ...) {standardGeneric("addLabel")})
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
#' \donttest{
#' arrows_ls <- list(gaugeArrow(value = 130), gaugeArrow(value = 150))
#' amAngularGaugeChart(arrows = arrows_ls)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setArrows", def = function(.Object, arrows = NULL) {standardGeneric("setArrows")})
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
#' @examples
#' \donttest{
#' addArrow(.Object = amAngularGaugeChart(), value = 10)
#' # equivalent to:
#' gaugeArrow_obj <- gaugeArrow(value = 10)
#' addArrow(.Object = amAngularGaugeChart(), arrow = gaugeArrow_obj)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addArrow", def = function(.Object, arrow = NULL, ...) { standardGeneric("addArrow") } )
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
#' \donttest{
#' axes_ls <- list(gaugeAxis(value = 130), gaugeAxis(value = 150))
#' setAxes(.Object = amAngularGaugeChart(), axes = axes_ls)
#' # If possible, simplify your code by using the constructor:
#' amAngularGaugeChart(axes = axes_ls)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setAxes", def = function(.Object, axes, ...) {standardGeneric("setAxes")})
#' @rdname initialize-AmChart
setMethod(f = "setAxes", signature = c("AmChart", "list"),
          definition = function(.Object, axes)
          {
            rightClassElements <- all(sapply(axes, function(element) {is(element, "GaugeAxis")}))
            if (!rightClassElements) {
              stop("[setAxes]: each element of axes must be of class GaugeAxis")
            } else{}
            .Object@axes <- lapply(axes, listProperties)
            validObject(.Object)
            return(.Object)
          })





#' @param axe (optional) \linkS4class{GaugeAxis}.
#' Argument of deprecated method \code{addAxe}.
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addAxe", def = function(.Object, axe = NULL, ...) {standardGeneric("addAxe")})
#' @rdname initialize-AmChart
setMethod(f = "addAxe", signature = c("AmChart", "GaugeAxisOrMissing"),
          definition = function(.Object, axe = NULL, ...)
          {
            if (is.null(axe) && !missing(...)) {
              axe <- gaugeAxis(...)
            } else if (is.null(axe) && missing(...)) {
              stop("You must either give 'axis' argement or its properties")
            } else {}
            
            .Object@axes <- rlist::list.append(.Object@axes, listProperties(axe))
            validObject(.Object)
            return(.Object)
          })

addAxis_def <- function(.Object, axis = NULL, ...)
{
  if (is.null(axis) && !missing(...)) {
    axis <- gaugeAxis(...)
  } else if (is.null(axis) && missing(...)) {
    stop("You must either give 'axe' argement or its properties")
  } else {}
  
  .Object@axes <- rlist::list.append(.Object@axes, listProperties(axis))
  validObject(.Object)
  return(.Object)
}

#' @param axis (optional) \linkS4class{GaugeAxis}.
#' same as axe.
#' @details 
#' Method 'addAxe' is deprecated, use 'addAxis'.
#' @examples
#' \donttest{
#' addAxis(.Object = amAngularGaugeChart(), startValue = 0, endValue = 100, valueInterval = 10)
#' # equivalent to:
#' gaugeAxis_obj <- gaugeAxis(startValue = 0, enValue = 100, valueInterval = 10)
#' addAxis(.Object = amAngularGaugeChart(), axis = gaugeAxis_obj)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addAxis", def = function(.Object, axis = NULL, ...) {standardGeneric("addAxis")})
#' @rdname initialize-AmChart
setMethod(f = "addAxis", signature = c("AmChart", "GaugeAxisOrMissing"),
          definition = addAxis_def)





#' @param amBalloon \linkS4class{AmBalloon}, argument of method 'setBalloon'.
#' @examples
#' \donttest{
#' setBalloon(.Object = amSerialChart(), adjustBorderColor = TRUE, fillColor = "#FFFFFF",
#'            color = "#000000", cornerRadius = 5)
#' # equivalent to:
#' amBalloon_obj <- amBalloon(adjustBorderColor = TRUE, fillColor = "#FFFFFF",
#'                            color = "#000000", cornerRadius = 5)
#' setBalloon(.Object = amSerialChart(), amBalloon = amBalloon_obj)
#' }
#' # ---
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
#' \donttest{
#' setCategoryAxis(.Object = amSerialChart(), gridPosition = "start")
#' # equivalent to:
#' categoryAxis_obj <- categoryAxis(gridPosition = "start")
#' setCategoryAxis(.Object = amSerialChart(), categoryAxis = categoryAxis_obj)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setCategoryAxis", def = function(.Object, categoryAxis = NULL , ...) {standardGeneric("setCategoryAxis")})
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
#' setCategoryField(.Object = amSerialChart(), categoryField = "country")
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setCategoryField", def = function(.Object, categoryField) {standardGeneric("setCategoryField")})
#' @rdname initialize-AmChart
setMethod(f = "setCategoryField", signature = c("AmChart", "character"),
          definition = function(.Object, categoryField)
          {
            .Object@categoryField <- categoryField
            validObject(.Object)
            return(.Object)
          })





#' @examples
#' \donttest{
#' # with default value, no argument needed
#' setChartCursor(.Object = amSerialChart())
#' # other example
#' setChartCursor(.Object = amSerialChart(), oneBallOnly = TRUE)
#' # equivalent to
#' chartCursor_obj <- chartCursor(oneBallOnly = TRUE)
#' setChartCursor(.Object = amSerialChart(), chartCursor = chartCursor_obj)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setChartCursor", def = function(.Object, chartCursor = NULL, ...) {standardGeneric("setChartCursor")})
#' @rdname initialize-AmChart
setMethod(f = "setChartCursor", signature = c("AmChart", "ChartCursorOrMissing"),
          definition = function(.Object, chartCursor = NULL, ...)
          {
            if (is.null(chartCursor) && !missing(...)) {
              chartCursor <- chartCursor(...)
            } else if (is.null(chartCursor) && missing(...)) {
              chartCursor <- chartCursor()
              # message("default 'chartCursor' added")
            } else {}
            
            .Object@chartCursor <- listProperties(chartCursor)
            validObject(.Object)
            return(.Object)
          })





#' @examples
#' \donttest{
#' # Add the default scrollbar
#' setChartScrollbar(.Object = amSerialChart())
#' # equivalent to:
#' chartScrollbar_obj <- chartScrollbar(updateOnReleaseOnly = FALSE)
#' setChartScrollbar(.Object = amSerialChart(), chartScrollbar = chartScrollbar_obj)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setChartScrollbar", def = function(.Object, chartScrollbar = NULL, ...) {standardGeneric("setChartScrollbar")})
#' @rdname initialize-AmChart
setMethod(f = "setChartScrollbar", signature = c("AmChart", "ChartScrollbarOrMissing"),
          definition = function(.Object, chartScrollbar = NULL, ...)
          {
            if (is.null(chartScrollbar) && !missing(...)) {
              chartScrollbar <- chartScrollbar(...)
            } else if (is.null(chartScrollbar) && missing(...)) {
              chartScrollbar <- chartScrollbar()
            } else {}
            
            .Object@chartScrollbar <- listProperties(chartScrollbar)
            validObject(.Object)
            return(.Object)
          })





#' @examples
#' \donttest{
#' setCreditsPosition(.Object = amPieChart(), creditsPosition = "top-right")
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setCreditsPosition", def = function(.Object, creditsPosition) {standardGeneric("setCreditsPosition")})
#' @rdname initialize-AmChart
setMethod(f = "setCreditsPosition", signature = c("AmChart", "character"),
          definition = function(.Object, creditsPosition)
          {
            .Object@creditsPosition <- creditsPosition
            validObject(.Object)
            return(.Object)
          })

#' @param url \code{character}.
#' @param format \code{character}.
#' @examples
#' \donttest{
#' setDataLoader(.Object = amSerialChart(), url = "data.json", format = "json")
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setDataLoader", def = function(.Object, url, format, ...) {standardGeneric("setDataLoader")})
#' @rdname initialize-AmChart
setMethod(f = "setDataLoader", signature = c("AmChart", "character", "character"),
          definition = function(.Object, url, format, ...)
          {
            .Object@otherProperties["dataLoader"] <- NULL
            .Object <- setProperties(.Object = .Object, dataLoader = list(url = url, format = format,...))
            validObject(.Object)
            return(.Object)
          })

#' @param keepNA
#' object of class \code{logical}, default \code{TRUE}.
#' Indicates if \code{NULL} values have to be kept or ignored. 
#' @examples
#' \donttest{
#' dataProvider_obj <- data.frame(key = c("FR", "US", "GER", "ENG", "IT" ),
#'                                value = round(runif(5, max = 100)))
#' setDataProvider(.Object = amPieChart(), dataProvider = dataProvider_obj)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setMethod(f = "setDataProvider", signature = c("AmChart", "ANY", "logicalOrMissing"),
          definition = function(.Object, dataProvider, keepNA = TRUE)
          {
            .Object@dataProvider <- .toList(.testFormatData(dataProvider), keepNA )
            validObject(.Object)
            return(.Object)
          })

# > @graphs : setters ####

#' @examples
#' \donttest{
#' graphs_ls <- list(graph(balloonText = "balloonText"), graph(type = "column"))
#' setGraphs(.Object = amSerialChart(), graphs = graphs_ls)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setGraphs", def = function(.Object, graphs) {standardGeneric("setGraphs")})
#' @rdname initialize-AmChart
setMethod(f = "setGraphs", signature = c("AmChart", "list"),
          definition = function(.Object, graphs)
          {
            rightClassElements <- all(sapply(graphs, function(element) {is(element, "AmGraph")}))
            stopifnot(rightClassElements)
            .Object@graphs <- lapply(graphs, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @param amGraph (optional) \linkS4class{AmGraph}.
#' @examples
#' \donttest{
#' addGraph(.Object = amSerialChart(), balloonText = "balloonText", "type" = "column")
#' # equivalent to
#' amGraph_obj <- amGraph(balloonText = "balloonText", "type" = "column")
#' addGraph(.Object = amSerialChart(), amGraph = amGraph_obj)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addGraph",
           def = function(.Object, amGraph = NULL, ...) {standardGeneric("addGraph")})
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
#' \donttest{
#' print(setGraph(.Object = amGanttChart(), id = "amGraph-1"))
#' # equivalent to:
#' amGraph_obj <- amGraph(id = "amGraph-1")
#' setGraph(.Object = amGanttChart(), amGraph = amGraph_obj)
#' }
#' # ---
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
#' \donttest{
#' guides_ls <- list(guide(fillAlpha = .1), guide(fillAlpha = .5))
#' amSerialChart(guides = guides_ls)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setGuides",
           def = function(.Object, guides) {standardGeneric("setGuides")})
#' @rdname initialize-AmChart
setMethod(f = "setGuides", signature = c("AmChart", "list"),
          definition = function(.Object, guides)
          {
            rightClassElements <- all(sapply(guides, function(element) {is(element, "Guide")}))
            if (rightClassElements) {
              .Object@guides <- lapply(guides, listProperties)
            } else {}
            validObject(.Object)
            return(.Object)
          })

#' @param guide (optional) \linkS4class{Guide}.
#' Argument of method \code{addGuide}.
#' @examples
#' addGuide(.Object = amSerialChart(), fillAlpha = .1, value = 0, toVAlue = 10)
#' # equivalent to
#' guide_obj <- guide(fillAlpha = .1, value = 0, toValue = 10, valueAxis = "1")
#' addGuide(.Object = amSerialChart(), guide = guide_obj)
#' # ---
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
#' \donttest{
#' setLegend(.Object = amChart(), amLegend = amLegend(useGraphSettings = TRUE))
#' # equivalent to:
#' setLegend(.Object = amChart(), useGraphSettings = TRUE)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setLegend",
           def = function(.Object, amLegend = NULL, ...) {standardGeneric("setLegend")})
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
#' \donttest{
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
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addSegment",
           def = function (.Object, categoryIDs, sgts) { standardGeneric("addSegment") })
#' @rdname initialize-AmChart
setMethod(f = "addSegment", signature = c( .Object = "AmChart", categoryIDs = "numeric"),
          definition = function(.Object, categoryIDs, sgts)
          {
            if (!all(categoryIDs %in% 1:length(.Object@dataProvider))) {
              stop( "[addSegment]: range of argument categoryIDs must in [", 
                    1 , ":" , length(.Object@dataProvider), "]" )
            } else {}
            
            if (!length(.Object@segmentsField)) {
              stop( "[addSegment]: The property segmentsField must be non NULL" )
            } else { segmentField <- as.character(.Object@segmentsField) }
            
            add <- function(.Object, categoryID, sgt) {
              
              if (is(sgt, "data.frame")) {
                #cat("data.frame")
                .Object@dataProvider[[eval(categoryID)]][eval(segmentField)] <<- list(.toList(sgt))
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
#' \donttest{
#' amChart_obj <- amChart(dataProvider = data.frame(a = 1:5, b = 6:10))
#' addSubData(.Object = amChart_obj, categoryIDs = 3, data = data.frame(a = 1:10, b = 11:20))
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addSubData",
           def = function(.Object, categoryIDs, data) {standardGeneric("addSubData")})
#' @rdname initialize-AmChart
setMethod(f = "addSubData", signature = c("AmChart", "numeric", "ANY"),
          definition = function(.Object, categoryIDs, data)
          {
            if (prod(categoryIDs %in% 1:length(.Object@dataProvider)) != 1) {
              stop( "[addSubData]: range of argument categoryIDs must in [", 
                    1 , ":" , length(.Object@dataProvider), "]" )
            } else {}
            
            add <- function(.Object, categoryID, data) {
              if (is(data, "data.frame")) {
                #cat("data.frame")
                data <- .testFormatData(data)
                .Object@dataProvider [[ eval(categoryID) ]] <- rlist::list.append( .Object@dataProvider[[ eval(categoryID) ]],
                                                                                   subdata = .toList(data) )
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
#' \donttest{
#' setSubChartProperties(.Object = amSerialChart(), type = "serial")
#' }
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
#' \donttest{
#' setTheme(.Object = amPieChart(), theme = "dark")
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setTheme", def = function(.Object, theme) {standardGeneric("setTheme")})
#' @rdname initialize-AmChart
setMethod(f = "setTheme", signature = c("AmChart", "character"),
          definition = function(.Object, theme)
          {
            .Object@theme <- theme
            validObject(.Object)
            return(.Object)
          })

#' @examples
#' \donttest{
#' titles_ls <- list(title(text = "balloonText"), title(text = "column"))
#' setTitles(.Object = amXYChart(), titles = titles_ls)
#' # or...
#' amXYChart(titles = titles_ls)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setTitles", def = function(.Object, titles) { standardGeneric("setTitles") } )
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
#' \donttest{
#' addTitle(.Object = amPieChart(), text = "balloonText", size = 15)
#' # equivalent to
#' title_obj <- title(text = "balloonText", size = 15)
#' addTitle(.Object = amPieChart(), title = title_obj)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addTitle",
           def = function(.Object, title = NULL, ...) {standardGeneric("addTitle")})
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
#' \donttest{
#' trendLines <- list(trendLine(initialValue = 1, finalValue = 5),
#'                    trendLine(initialValue = 7, finalValue = 19))
#' setTrendLines(.Object = amSerialChart(), trendLines = trendLines)
#' # or... 
#' amSerialChart(trendLines = trendLines) # Equivalent
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setTrendLines",
           def = function(.Object, trendLines) {standardGeneric("setTrendLines")})
#' @rdname initialize-AmChart
setMethod(f = "setTrendLines", signature = c("AmChart", "list"),
          definition = function(.Object, trendLines)
          {
            rightClassElements <- all(sapply(trendLines, function(element) {class(element) == "TrendLine"}))
            stopifnot(rightClassElements)
            .Object@trendLines <- lapply(trendLines, listProperties)
            validObject(.Object)
            return(.Object)
          })

#' @param trendLine (optional) \linkS4class{TrendLine}.
#' Argument of method \code{addTrendLine}.
#' @examples 
#' \donttest{
#' addTrendLine(.Object = amSerialChart(), initialValue = 1, initialXValue = 1,
#'              finalValue = 11, finalXValue = 12)
#' # equivalent to:
#' trendLine_obj <- trendLine(initialValue = 1, initialXValue = 1, finalValue = 11, finalXValue = 12)
#' addTrendLine(.Object = amSerialChart(), trendLine = trendLine_obj)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addTrendLine",
           def = function(.Object, trendLine = NULL, ...) {standardGeneric("addTrendLine")})
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
#' \donttest{
#' setType(.Object = amChart(), type = "pie")
#' # equivalent to:
#' amPieChart()
#' }
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
#' \donttest{
#' valueAxes <- list(valueAxis(axisTitleOffset = 12, tickLength = 10),
#'                   valueAxis(axisTitleOffset = 10, tickLength = 10))
#' setValueAxes(.Object = amSerialChart(), valueAxes = valueAxes)
#' # or...
#' amSerialChart(valueAxes = valueAxes)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "setValueAxes",
           def = function(.Object, valueAxes) {standardGeneric("setValueAxes")})
#' @rdname initialize-AmChart
setMethod(f = "setValueAxes", signature = c("AmChart", "list"),
          definition = function(.Object, valueAxes)
          {
            rightClassElements <- all(sapply(valueAxes, function(element) {is(element, "ValueAxis")}))
            stopifnot(rightClassElements)
            .Object@valueAxes <- lapply(valueAxes, listProperties)
            validObject(.Object)
            return(.Object)
          })

# ---

addValueAxis_def <- function(.Object, valueAxis = NULL, ...)
{
  if (is.null(valueAxis) && !missing(...)) {
    valueAxis <- valueAxis(...)
  } else if (is.null(valueAxis) && missing(...)) {
    stop("You must provide argument 'valueAxis' or its properties")
  } else {}
  
  .Object@valueAxes <- rlist::list.append(.Object@valueAxes, listProperties(valueAxis))
  validObject(.Object)
  return(.Object)
}

#' @details For method \code{addValueAxis}: valueAxis is optional. Method \code{addValueAxes} is deprecated.
#' @examples
#' \donttest{
#' addValueAxis(.Object = amSerialChart(), axisTitleOffset = 12, tickLength = 10)
#' # equivalent to:
#' valueAxis_obj <- valueAxis(axisTitleOffset = 12, tickLength = 10)
#' addValueAxis(.Object = amSerialChart(), valueAxis = valueAxis_obj)
#' }
#' # ---
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addValueAxes",
           def = function(.Object, valueAxis = NULL, ... ) {standardGeneric("addValueAxes")})
#' @rdname initialize-AmChart
setMethod(f = "addValueAxes", signature = c("AmChart", "ValueAxisOrMissing"),
          definition = addValueAxis_def)
#' @rdname initialize-AmChart
#' @export
setGeneric(name = "addValueAxis", def = function(.Object, valueAxis = NULL, ... ) { standardGeneric("addValueAxis") } )
#' @rdname initialize-AmChart
setMethod(f = "addValueAxis", signature = c("AmChart", "ValueAxisOrMissing"),
          definition = addValueAxis_def)

#' @details Method \code{setValueAxis} is only valid for Gantt charts.
#' @examples
#' \donttest{
#' setValueAxis(.Object = amGanttChart())
#' setValueAxis(.Object = amGanttChart(), type = "date")
#' }
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
