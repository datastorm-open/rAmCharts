#' @title Shared generics methods
#' @details \code{setBalloon} is shared by AmChart and AmStockChart
#' @param .Object \linkS4class{AmObject}.
#' @param amBalloon \linkS4class{AmBalloon}.
#' @rdname shared-generics
#' @export
setGeneric(name = "setBalloon", def = function(.Object, amBalloon = NULL, ...) {standardGeneric("setBalloon")})

#' @details setDataProvider(..) is shared by AmGraph and DataSet
#' @param dataProvider \code{data.frame}.
#' @param keepNA \code{logical}, default \code{TRUE}.
#' Indicates if \code{NULL} values have to be kept or ignored. 
#' @rdname shared-generics
#' @export
setGeneric(name = "setDataProvider",
           def = function(.Object, dataProvider, keepNA = TRUE) { standardGeneric("setDataProvider") } )

#' @details setTitle(...) is Shared by AmGraph and ValueAxis
#' @param title \code{character}.
#' @rdname shared-generics
#' @export
setGeneric( name = "setTitle", def = function(.Object, title){ standardGeneric("setTitle") } )

#' @details setType(...) is shared by AmGraph and AmChart
#' @param type \code{character}.
#' @rdname shared-generics
#' @export
setGeneric( name = "setType", def = function(.Object, type) { standardGeneric("setType") } )

#' @details setGraph(...) is shared by AmChart and ChartScrollbar
#' @param graph \linkS4class{AmGraph}.
#' @param ... Other properties.
#' @rdname shared-generics
#' @export
setGeneric( name = "setGraph",
            def = function(.Object, graph = NULL, ...) {standardGeneric("setGraph")} )


#' @details addGuide(...) is shared by AxisBase and AmChart
#' @param guide \linkS4class{Guide}.
#' @rdname shared-generics
#' @export
setGeneric(name = "addGuide", def = function(.Object, guide = NULL, ...){ standardGeneric("addGuide") } )

#' @details setText(...) is shared by Title and Label
#' @param text \code{character}.
#' @rdname shared-generics
#' @export
setGeneric(name = "setText", def = function(.Object, text){ standardGeneric("setText") } )

#' @details setValueAxis(...) is shared by AmChart(type = "gantt"), TrendLine and Guide
#' @param valueAxis \linkS4class{ValueAxis}.
#' @rdname shared-generics
#' @export
setGeneric(name = "setValueAxis",
           def = function(.Object, valueAxis = NULL, ...){ standardGeneric("setValueAxis") } )

#' @details addValueAxis(...) is shared by TrendLine and Guide
#' @rdname shared-generics
#' @export
setGeneric( name = "addValueAxis",
            def = function(.Object, valueAxis = NULL, ... ) { standardGeneric("addValueAxis") } )
