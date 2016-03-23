#' @title amOptions
#' @description amOptions sets the most common options for chart customization.
#' You can set other properties with the method \link{setProperties}.
#' See details for exception.
#' 
#' @param chart \linkS4class{AmChart}.
#' @param legend \code{logical}, default \code{FALSE}. TRUE to add a legend to the chart.
#' @param legendPosition \code{character}, possible values are : "left", "right", "top" or "bottom", 
#' default set to "right".
#' @param legendAlign \code{character}, controls the legend alignement. Possible values are : 
#' "left","right" or "center", default set to "left". Only used if \code{legend = TRUE}.
#' @param export \code{logical}, default set to  FALSE. TRUE to display export feature.
#' @param exportFormat \code{character}, desired export format. Possible values are : "JPG",
#'  "PNG" ,"SVG", "CSV", "JSON", "PDF", "XLSX", "PRINT". 
#' @param creditsPosition \code{character}, controsl credits position. Possible values are : 
#' "top-left", "top-right", "bottom-left" or "bottom-right", default set to "top-left".
#' @param theme \code{character}, possible values are : "none", "light", "dark", "patterns",
#' "chalk", default set to "none".
#' @param main \code{character}, chart's title.
#' @param mainColor \code{character}, main color (in hexadecimal), default set to "#000000".
#' @param mainSize \code{numeric}, main size, default set to 15.
#' @param zoom \code{logical}, TRUE to add a chart cursor, default set to FALSE.
#' @param scrollbar \code{logical}, default \code{FALSE}, TRUE to display scrollbar.
#' @param scrollbarHeight \code{numeric}, height in pixels, must be > 0.
#' @param labelRotation \code{numeric}, rotation angle of a label. Only horizontal axis' values 
#' can be rotated. Value must be between -90 and 90.
#' 
#' @details
#' \strong{Exception:} 
#' \itemize{
#'  \item{It's not possible to export a gauge chart data as CSV.}
#'  }
#' 
#' @import pipeR
#' 
#' @examples
#' \donttest{
#' library(pipeR)
#' data(data_pie) 
#' 
#' # Export                 
#' amPie(data = data_pie) %>>%
#'   amOptions(export = TRUE)
#' 
#' # Legend
#' amPie(data = data_pie) %>>%
#'   amOptions(legend = TRUE)
#' 
#' # Legend position
#' amPie(data = data_pie) %>>%
#'   amOptions(legend = TRUE, legendPosition = "bottom")
#' 
#' # Credits position
#' amPie(data = data_pie) %>>%
#'   amOptions(creditsPosition = "bottom-right")
#' 
#' # Theme
#' amPie(data = data_pie) %>>%
#'   amOptions(theme = "chalk")
#' 
#' # Title
#' amPie(data = data_pie) %>>%
#'   amOptions(main = "Social network", mainColor = "#FFFFFF", mainSize = 40, theme = "chalk")
#' 
#' 
#' # Custom exemple
#' amPie(data = data_pie) %>>%
#'   amOptions(main = "Social network", mainColor = "#FFFFFF", mainSize = 40,
#'             theme = "dark", legend = TRUE, legendPosition = "bottom",
#'             creditsPosition = "bottom-right" )
#' }
#' 
#' @seealso 
#' \itemize{
#' \item{\url{https://datastorm-open.github.io/introduction_ramcharts/}}
#' }
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
      message("Impossible to a legend on a gauge chart")
    } else {
      if (length(chart@otherProperties$RType_) > 0) {
        
        if (chart@otherProperties$RType_ %in% c("candlestick", "waterfall",
                                                "boxplot", "histogram"))
        {
          message("You cannot add a legend this kind of chart")
        }
        
        
        if (chart@otherProperties$RType_ %in% c("barplot", "wind"))
        {
          if(length(chart@graphs) == 1)
          {
            legend <- chart@dataProvider
            if(length(unique(unlist(legend)[names(unlist(legend)) == "color"])) > 1)
            {
              
              lapply(1:length(legend), function(X){
                names(legend[[X]])[names(legend[[X]]) == chart@categoryField] <<- "title"
              })
              
            } else{
              
              legend <- list(list(title = chart@categoryField,
                                  color = unique(unlist(legend)[names(unlist(legend)) == "color"])))
              
            }
            
            chart <- setLegend(.Object = chart, position = legendPosition, data = legend)
          } else {
            chart <- setLegend(.Object = chart, position = legendPosition,
                               useGraphSettings = TRUE, align = legendAlign)
          }
          
        }
        
        
      } else {
        if (chart@type %in% c("radar", "serial", "xy")) {
          chart <- setLegend(.Object = chart, position = legendPosition,
                             useGraphSettings = TRUE, align = legendAlign)
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
      # text compatibility of formats
      if ("CSV" %in% exportFormat && chart@type == "gauge") {
        message("Export 'CSV' impossible for type gauge")
        exportFormat <- setdiff(x = exportFormat, y = "CSV")
      }
      
      # test if there are some other formats
      if (length(exportFormat)) {  
        exportMenu <- lapply(exportFormat, function(format) {
          list(format = format, label = paste("Download as", format), title = format)
        })
        chart <- setExport(.Object = chart, enabled = TRUE,
                           menu = list(list(class = "export-main", menu = exportMenu)))
      }
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
  if (zoom)
    chart <- setChartCursor(.Object = chart)
  else
    slot(object = chart, name = "chartCursor", check = TRUE) <- list()
  
  ## Set scrollbar
  if (scrollbar) 
    chart <- setChartScrollbar(.Object = chart, enabled = scrollbar, scrollbarHeight = scrollbarHeight)
  else 
    slot(object = chart, name = "chartScrollbar", check = TRUE) <- list()
  
  ## Rotate label
  if (labelRotation !=0) {
    if (!chart@type == "radar")
      chart <- setCategoryAxis(.Object = chart, labelRotation = labelRotation)
    else
      message("Impossible to rotate label for a radar chart !")
  }
  
  chart
}
