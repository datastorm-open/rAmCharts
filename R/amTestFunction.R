.testAmOptions <- function(chart){
  #test legend
  amOptions(chart, legend = TRUE)
  amOptions(chart, legend = TRUE, legendPosision = "bottom")
  amOptions(chart, legend = TRUE, legendPosision = "bottom", legendAlign = "center")
  
  #Export
  amOptions(chart, export = TRUE)
  amOptions(chart, export = TRUE, exportFormat = c("CSV","JPG"))
  
  #creditsPosition
  amOptions(chart, creditsPosition = "bottom-right")
  
  #theme
  amOptions(chart, theme  = "chalk")
  
  #main
  amOptions(chart, main  = "My plot")
  amOptions(chart, main  = "My plot", mainColor = "#BBBBBB")
  amOptions(chart, main  = "My plot", mainColor = "#BBBBBB", mainSize = 50)
  TRUE
}