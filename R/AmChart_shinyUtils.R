#' @exportMethod amChartOutput
setGeneric( name = "amChartOutput",
  def = function(outputId, type, width, height) { standardGeneric("amChartOutput") }
)

#' @title SHINY
#' @description Widget output function for use in Shiny
#' @param amChart: an \code{\linkS4class{AmChart}} object
#' @name amChartOutput
#' @rdname amChartOutput
#' @export
setMethod(
  f = "amChartOutput",
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
    
    shinyWidgetOutput(
      outputId = outputId,     
      name = eval(jsFile),   
      width = width,
      height = height,
      package = 'rAmCharts'
    )
  }
)

#' @exportMethod amChartOutput
setGeneric(
  name = "renderAmChart",
  def = function(expr, env, quoted) { standardGeneric("renderAmChart") }
)

#' @title SHINY
#' @description Widget output function for use in Shiny
#' @param amChart: an \code{\linkS4class{AmChart}} object
#' @name renderAmChart
#' @rdname renderAmChart
#' @export
setMethod(
  f = "renderAmChart",
  definition = function(expr, env, quoted)
  {
    if(missing(env)){
      env <- parent.frame()
    }else{}
    if(missing(quoted)){
      quoted <- FALSE
    }
    if (!quoted) { expr <- substitute(expr) } # force quoted
    shinyRenderWidget(expr, amChartOutput, env, quoted = TRUE)
  }
)