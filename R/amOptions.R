#' @title amOptions
#' @author DataKnowledge
#' @description Most used options fot rAmCharts graphs customization
#' 
#' @param chart \linkS4class{AmChart}.
#' @param legend \code{boolean}, TRUE or FALSE, default FALSE, display legend if TRUE.
#' @param legendPosition \code{character}, control legend position,
#' can be "left", "right", "top" or "bottom", default "rigth". Only use if legend = TRUE.
#' @param legendAlign \code{character} control legend align, must be "left","rigth" or "center",
#' default "left". Only use if legend = TRUE.
#' @param export \code{boolean}, TRUE or FALSE, default FALSE, display export if TRUE
#' @param exportFormat \code{character} export format to keep, must be in JPG, PNG ,SVG,
#' CSV ,JSON, PDF, XLSX, PRINT
#' @param creditsPosition \code{character},  control credits position,
#' can be "top-left", "top-right", "bottom-left" or "bottom-right", default top-left
#' @param theme \code{character}, control theme.Can be "none","light","dark","patterns","chalk",
#' default "none
#' @param main \code{character}, title of graphic, default ""
#' @param mainColor \code{character}, color of title (html-color), default "#000000"
#' @param mainSize \code{numeric}, color of title (html-color), default 15
#' @param zoom \code{boolean}, activated zoom, default FALSE.
#' @param scrollbar \code{boolean}, TRUE or FALSE, default FALSE, display Scrollbar if TRUE
#' @param scrollbarHeight \code{numeric}, Height in px, must be >0
#' @param labelRotation \code{numeric} Rotation angle of a label. Only horizontal axis' values can be rotated.
#' Must be between -90 and 90.
#' @param ... Don't use...
#' 
#' @import pipeR
#' 
#' @example examples/amOptions_examples.R
#' 
#' 
#' @rdname amOptions
#' @export
amOptions <- function(chart, legend = FALSE,legendPosition = "right", legendAlign = "left",
                      export = FALSE, exportFormat = NULL, creditsPosition = "top-left", theme = "none",
                      main = "", mainColor = "#000000", mainSize = 15, 
                      zoom = FALSE, scrollbar = FALSE, scrollbarHeight = 20,
                      labelRotation = 0,  ...) {
  #Control
  
  #Legend
  .testLogicalLength1(logi = legend)
  
  #legendPosition
  .testCharacter(char = legendPosition)
  .testIn(vect = legendPosition, control = c("left", "right", "top", "bottom"))
  
  #legendAlign
  .testCharacterLength1(char = legendAlign)
  .testIn(vect = legendAlign, control = c("left", "rigth", "center"))
  
  #Export
  .testLogicalLength1(logi = export)
  
  #ExportFormat
  .testIn(vect = exportFormat, control = c("JPG", "PNG" ,"SVG", "CSV" ,"JSON", "PDF", "XLSX", "PRINT"))
  
  #creditsPosition
  .testIn(vect = creditsPosition, control = c("top-left", "top-right", "bottom-left", "bottom-right"))
  
  #theme
  .testIn(vect = theme, control = c("none", "light", "dark", "patterns", "chalk")) 
  
  #main
  .testCharacterLength1(char = main)
  
  #mainColor
  .testCharacterLength1(char = mainColor)
  
  #mainsize
  .testNumericLength1(num = mainSize)
  
  #zoom
  .testLogicalLength1(logi = zoom)
  
  #scrollbar
  .testLogicalLength1(logi = scrollbar)
  
  #scrollbarHeight
  .testNumericLength1(num = scrollbarHeight)
  .testInterval(num = scrollbarHeight, binf = 0)
  
  #labelRotation
  .testNumericLength1(num = labelRotation)
  .testInterval(num = labelRotation,binf = -90, bsup = 90)

  #Set legend to graph, usage of useGraphSettings argument depend of graph type
  if (legend == TRUE)
  {
    
    if(chart@type == "gauge")
    {
      warning("Legend argument is not logic for a Gauge chart, it is remove")
    }else{
    if(chart@type%in%c("radar","serial","xy"))
    {
      chart <- chart %>>% setLegend(position = legendPosition, useGraphSettings = TRUE, align = legendAlign)
    }else{
      chart <- chart %>>% setLegend(position = legendPosition, align = legendAlign)
    }}
  }
  
  ###Export
  
  #Set export
  if (export == TRUE)
  {
    if (is.null(exportFormat))
    {
    chart <- chart %>>% setProperties(export = list(enabled = TRUE))
    }else{
      menuc <- sapply(exportFormat, function(X){
        list(format = X, label = X)}, simplify = FALSE, USE.NAMES = FALSE)

        chart <- chart %>>% setProperties(export = list(enabled = TRUE, menu = list(list(class = "export-main" , menu = menuc))))
    }
  }
  
  if(creditsPosition != "top-left")
  {
    chart <- chart %>>% setProperties(creditsPosition = creditsPosition)
  }
  
  
  ##Set theme
  if(theme != "none")
  {
    chart <- chart %>>% setProperties(theme =  theme)
  }
  
  main <- as.character(main)
  if(main!="")
  {
    chart <- chart %>>% addTitle(text =  main, size = mainSize, color = mainColor)
  }
  
  
  if(zoom)
  {
    chart <- chart %>>% setChartCursor()
  }
  
  if(scrollbar)
  { 
    chart <- chart %>>% setChartScrollbar(enabled = scrollbar, scrollbarHeight = scrollbarHeight)
  }
  
  if(labelRotation !=0)
  {
    chart <- chart %>>% setCategoryAxis(labelRotation = labelRotation)
  }
  
  chart
}


