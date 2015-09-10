#' @exportMethod amChartsOutput
setGeneric( name = "amChartsOutput",
            def = function(outputId, type, width, height) { standardGeneric("amChartsOutput") }
)

#' @title SHINY
#' @description Widget output function for use in Shiny
#' @param amChart: an \code{\linkS4class{AmChart}} object
#' @rdname amChartsOutput
#' @import htmlwidgets
setMethod(
  f = "amChartsOutput",
  signature = c("character", "character"),
  definition = function(outputId, type, width, height)
  {
    if(missing(width)){
      width <- "100%"
    }else{}
    if(missing(height)){
      height <- "400px"
    }
    
    jsFile <- switch(type,
                     "funnel" = "amFunnelChart",
                     "gantt" = "amGanttChart",
                     "gauge" = "amAngularGauge",
                     "pie" = "amPieChart",
                     "radar" = "amRadarChart",
                     "serial" = "amSerialChart",
                     "stock" = "amStockChart",
                     "xy" = "amXYChart",
                     "drill" = "amDrillChart",
                     stop("type error")
    )
    
    htmlwidgets::shinyWidgetOutput(
      outputId = outputId,     
      name = eval(jsFile),   
      width = width,
      height = height,
      package = 'rAmCharts'
    )
  }
)



#' @title SHINY
#' @description Widget output function for use in Shiny
#' @param amChart: an \code{\linkS4class{AmChart}} object
#' @rdname renderAmCharts
#' @export
renderAmCharts <- function(expr, env, quoted){
  if(missing(env)){
    env <- parent.frame()
  }else{}
  if(missing(quoted)){
    quoted <- FALSE
  }
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, amChartsOutput, env, quoted = TRUE)
}
