# Shared by AmGraph and DataSet
#' @exportMethod setDataProvider
setGeneric( name = "setDataProvider",
            def = function(.Object, dataProvider, keepNA = TRUE) { standardGeneric("setDataProvider") } )

# Shared by AmGraph and ValueAxis
#' @exportMethod setTitle
setGeneric( name = "setTitle", def = function(.Object, title){ standardGeneric("setTitle") } )

# Shared by AmGraph and AmChart
#' @exportMethod setType
setGeneric( name = "setType", def = function(.Object, type) { standardGeneric("setType") } )

# Shared by AmChart and ChartScrollbar
#' @exportMethod setGraph
setGeneric( name = "setGraph",
            def = function(.Object, graph = NULL, ...) {standardGeneric("setGraph")} )


# Shared by AxisBase and AmChart
#' @exportMethod addGuide
setGeneric(name = "addGuide", def = function(.Object, guide = NULL, ...){ standardGeneric("addGuide") } )

# Shared by Title and Label
#' @exportMethod setText
setGeneric(name = "setText", def = function(.Object, text){ standardGeneric("setText") } )

# Shared by AmChart(type = "gantt"), TrendLine and Guide
#' @exportMethod setValueAxis
setGeneric(name = "setValueAxis",
           def = function(.Object, valueAxis = NULL, ...){ standardGeneric("setValueAxis") } )

# Shared by TrendLine and Guide
#' @exportMethod addValueAxis
setGeneric( name = "addValueAxis",
            def = function(.Object, valueAxis = NULL, ... ) { standardGeneric("addValueAxis") } )
