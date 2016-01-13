#' @title amOptions
#' @author DataKnowledge
#' @description Most used options fot rAmCharts graphs customization
#' 
#' @param chart \linkS4class{AmChart}.
#' @param legend \code{boolean}, TRUE or FALSE, default FALSE, display legend if TRUE.
#' @param legendPosision \code{character}, control legend position,
#' can be "left", "right", "top" or "bottom", default "rigth". Only use if legend = TRUE.
#' @param export \code{boolean}, TRUE or FALSE, default FALSE, display export if TRUE
#' @param creditsPosition \code{character},  control credits position,
#' can be "top-left", "top-right", "bottom-left" or "bottom-right", default top-left
#' @param theme \code{character}, control theme.Can be "none","light","dark","patterns","chalk",
#' default "none
#' @param main \code{character}, title of graphic, default ""
#' @param mainColor \code{character}, color of title (html-color), default "#000000"
#' @param mainSize \code{numeric}, color of title (html-color), default 15
#' @param ... Don't use...
#' 
#' @import pipeR
#' 
#' @examples
#' 
#' 
#' data_pie <- data.frame(label = c("Facebook", "Twitter", "LinkedIn", "Google+", 
#'                                  "Pinterest"),
#'                        value = c(38, 25, 15, 14, 8), stringsAsFactors = FALSE)
#' 
#' library(pipeR)
#' #Export                 
#' amPie(data = data_pie)%>>%
#'   amOptions(export = TRUE)

#' #Legend
#' amPie(data = data_pie)%>>%
#'   amOptions(legend = TRUE)
#' 
#' #Legend position
#' amPie(data = data_pie)%>>%
#'   amOptions(legend = TRUE, legendPosision = "bottom")
#' 
#' #credits Position
#' amPie(data = data_pie)%>>%
#'   amOptions(creditsPosition = "bottom-right")
#' 
#' #credits Position
#' amPie(data = data_pie)%>>%
#'   amOptions(creditsPosition = "bottom-right")
#' 
#' #Theme
#' amPie(data = data_pie)%>>%
#'   amOptions(theme = "chalk")
#' 
#' #Title
#' amPie(data = data_pie)%>>%
#'   amOptions(main = "Social network", mainColor = "#FFFFFF", mainSize = 40, theme = "chalk")
#' 
#' 
#' #custom exemple
#' amPie(data = data_pie)%>>%
#'   amOptions(main = "Social network", mainColor = "#FFFFFF", mainSize = 40,
#'             theme = "dark", legend = TRUE, legendPosision = "bottom",
#'              creditsPosition = "bottom-right" )
#' 
#' 
#' 
#' @rdname amOptions
#' @export
amOptions <- function(chart,legend = FALSE,legendPosision = "right", 
                      export = FALSE, creditsPosition = "top-left",theme = "none",main = "",mainColor = "#000000", mainSize = 15,  ...) {
  ###Legend
  
  #Controle if userinput are available
  if(!legend%in%c(TRUE, FALSE))
  {
    stop("legend must be TRUE or FALSE")
  }
  
  if(!legendPosision%in%c("left", "right", "top", "bottom"))
  {
    stop('legendPosision must be "left", "right", "top" or "bottom"')
  }

  #Set legend to graph, usage of useGraphSettings argument depend of graph type
  if(legend == TRUE)
  {
    if(chart@type%in%c("radar","serial","xy"))
    {
    chart <- chart %>>% setLegend(position = legendPosision, useGraphSettings = TRUE)
    }else{
    chart <- chart %>>% setLegend(position = legendPosision)
    }
  }
  
  ###Export
  #Controle if userinput are available
  if(!export%in%c(TRUE, FALSE))
  {
    stop("export must be TRUE or FALSE")
  }
  #Set export
  if(export == TRUE)
  {
  
    chart <- chart %>>% setProperties(export = list(enabled = TRUE))
  }
  
  
  ###creditsPosition
  if(!creditsPosition%in%c("top-left", "top-right", "bottom-left", "bottom-right"))
  {
    stop('creditsPosition must be "top-left", "top-right", "bottom-left" or "bottom-right"')
  }
  
  chart <- chart %>>% setProperties(creditsPosition =  creditsPosition)
  
  ###Theme
  if(!theme%in%c("none","light","dark","patterns","chalk"))
  {
    stop('theme must be "none","light","dark","patterns","chalk"')
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
  
  chart
}


