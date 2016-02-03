#' @title amOptions
#' @description Most used options fot rAmCharts graphs customization
#' 
#' @param chart \linkS4class{AmChart}.
#' @param legend \code{boolean}, TRUE or FALSE, default FALSE, display legend if TRUE.
#' @param legendPosition \code{character}, control legend position,
#' can be "left", "right", "top" or "bottom", default "rigth". Only use if legend = TRUE.
#' @param legendAlign \code{character} control legend align, must be "left","rigth" or "center",
#' default "left". Only use if legend = TRUE.
#' @param export \code{boolean}, TRUE or FALSE, default FALSE, display export if TRUE.
#' @param exportFormat \code{character} export format to keep, must be in JPG, PNG ,SVG,
#' CSV, JSON, PDF, XLSX, PRINT. Notice that it's impossible to export a gauge chart data as CSV. 
#' @param creditsPosition \code{character},  control credits position,
#' can be "top-left", "top-right", "bottom-left" or "bottom-right", default top-left.
#' @param theme \code{character}, control theme. Can be "none","light","dark","patterns","chalk",
#' default "none".
#' @param main \code{character}, title of graphic, default empty \code{character}.
#' @param mainColor \code{character}, color of title (html-color), default "#000000".
#' @param mainSize \code{numeric}, color of title (html-color), default 15.
#' @param zoom \code{boolean}, activated zoom, default FALSE.
#' @param scrollbar \code{boolean}, TRUE or FALSE, default FALSE, display Scrollbar if TRUE.
#' @param scrollbarHeight \code{numeric}, Height in px, must be > 0.
#' @param labelRotation \code{numeric} Rotation angle of a label. Only horizontal axis' values can be rotated.
#' Must be between -90 and 90.
#' 
#' @import pipeR
#' 
#' @example examples/amOptions_examples.R
#' 
#' 
#' @rdname amOptions
#' @export
#' 
amOptions <- function(chart, theme = "none", legend = FALSE, legendPosition = "right",
                      legendAlign = "left", export = FALSE, exportFormat = character(),
                      creditsPosition = "top-left", main = character(), mainColor = "#000000",
                      mainSize = 15, zoom = FALSE, scrollbar = FALSE, scrollbarHeight = 20,
                      labelRotation = 0)
{
  # Control parameters
  {
    # legend
    .testLogicalLength1(logi = legend)
    
    # legendPosition
    .testCharacter(char = legendPosition)
    .testIn(vect = legendPosition, control = c("left", "right", "top", "bottom"))
    
    # legendAlign
    .testCharacterLength1(char = legendAlign)
    .testIn(vect = legendAlign, control = c("left", "rigth", "center"))
    
    # export
    .testLogicalLength1(logi = export)
    
    # ExportFormat
    if (length(exportFormat))
      .testIn(vect = exportFormat, control = c("JPG", "PNG" ,"SVG", "CSV" ,"JSON", "PDF", "XLSX", "PRINT"))
    
    # creditsPosition
    .testIn(vect = creditsPosition, control = c("top-left", "top-right", "bottom-left", "bottom-right"))
    
    # theme
    .testIn(vect = theme, control = c("none", "light", "dark", "patterns", "chalk")) 
    
    # main
    if (length(main)) .testCharacterLength1(char = main)
    
    # mainColor
    .testCharacterLength1(char = mainColor)
    
    # mainsize
    .testNumericLength1(num = mainSize)
    
    # zoom
    .testLogicalLength1(logi = zoom)
    
    # scrollbar
    .testLogicalLength1(logi = scrollbar)
    
    # scrollbarHeight
    .testNumericLength1(num = scrollbarHeight)
    .testInterval(num = scrollbarHeight, binf = 0)
    
    # labelRotation
    .testNumericLength1(num = labelRotation)
    .testInterval(num = labelRotation,binf = -90, bsup = 90)
  }
  
  # Set legend to graph, usage of useGraphSettings argument depend of graph type
  if (legend) {
    if (chart@type == "gauge") {
      message("You cannot add a legend to a 'gauge' chart")
    } else {
      if (length(chart@otherProperties$RType_) > 0) {
        if (chart@otherProperties$RType_ %in% c("candlestick", "waterfall", "boxplot", "histogram"))
          message("You cannot add a legend this kind of chart")
      } else {
        if (chart@type %in% c("radar","serial","xy")) {
          chart <- setLegend(.Object = chart, position = legendPosition, useGraphSettings = TRUE, align = legendAlign)
        } else {
          chart <- setLegend(.Object = chart, position = legendPosition, align = legendAlign)
        }
      }
    }
  }

  ## Set export
  if (export || length(exportFormat)) {
    if (!length(exportFormat)) {
      chart <- setExport(.Object = chart)
    } else {
      menuc <- sapply(exportFormat, function(X) list(format = X, label = X), simplify = FALSE, USE.NAMES = FALSE)
      chart <- setExport(.Object = chart, enabled = TRUE, menu = list(list(class = "export-main" , menu = menuc)))
    }
  }
  
  ## Set creditsPosition
  if (creditsPosition != "top-left")
    chart <- setCreditsPosition(.Object = chart, creditsPosition = creditsPosition)
  
  ## Change theme
  if (theme != "none")
    chart <- setTheme(.Object = chart, theme =  theme)
  
  ## Set main (title)
  if (length(main)) 
    chart <- addTitle(.Object = chart, text =  main, size = mainSize, color = mainColor)
  
  ## Set zoom cursor
  if (zoom) chart <- setChartCursor(.Object = chart)
  
  ## Set scrollbar
  if (scrollbar) 
    chart <- setChartScrollbar(.Object = chart, enabled = scrollbar, scrollbarHeight = scrollbarHeight)
  
  ## Rotate label
  if (labelRotation !=0) {
    if (!chart@type %in% c("radar")) {
      chart <- setCategoryAxis(.Object = chart, labelRotation = labelRotation)
    } else {
      message("Impossible to rotate label for a 'radar' chart !")
    }
  }
  
  chart
}


