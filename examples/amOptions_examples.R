data_pie <- data.frame(label = c("Facebook", "Twitter", "LinkedIn", "Google+", 
                                 "Pinterest"),
                       value = c(38, 25, 15, 14, 8), stringsAsFactors = FALSE)

library(pipeR)
#Export                 
amPie(data = data_pie)%>>%
  amOptions(export = TRUE)

#Legend
amPie(data = data_pie)%>>%
  amOptions(legend = TRUE)

#Legend position
amPie(data = data_pie)%>>%
  amOptions(legend = TRUE, legendPosision = "bottom")

#credits Position
amPie(data = data_pie)%>>%
  amOptions(creditsPosition = "bottom-right")

#credits Position
amPie(data = data_pie)%>>%
  amOptions(creditsPosition = "bottom-right")

#Theme
amPie(data = data_pie)%>>%
  amOptions(theme = "chalk")

#Title
amPie(data = data_pie)%>>%
  amOptions(main = "Social network", mainColor = "#FFFFFF", mainSize = 40, theme = "chalk")


#custom exemple
amPie(data = data_pie)%>>%
  amOptions(main = "Social network", mainColor = "#FFFFFF", mainSize = 40,
            theme = "dark", legend = TRUE, legendPosision = "bottom",
             creditsPosition = "bottom-right" )
