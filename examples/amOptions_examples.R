data(data_pie) 

library(pipeR)
#Export                 
amPie(data = data_pie)%>>%
  amOptions(export = TRUE)

#Legend
amPie(data = data_pie)%>>%
  amOptions(legend = TRUE)

#Legend position
amPie(data = data_pie)%>>%
  amOptions(legend = TRUE, legendPosition = "bottom")

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
            theme = "dark", legend = TRUE, legendPosition = "bottom",
             creditsPosition = "bottom-right" )
