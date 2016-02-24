library(pipeR)

# Load data
data(data_pie) 

# Export                 
amPie(data = data_pie)%>>%
  amOptions(export = TRUE)

# Legend
amPie(data = data_pie)%>>%
  amOptions(legend = TRUE)

# Legend position
amPie(data = data_pie)%>>%
  amOptions(legend = TRUE, legendPosition = "bottom")

# Credits position
amPie(data = data_pie)%>>%
  amOptions(creditsPosition = "bottom-right")

# Theme
amPie(data = data_pie)%>>%
  amOptions(theme = "chalk")

# Title
amPie(data = data_pie)%>>%
  amOptions(main = "Social network", mainColor = "#FFFFFF", mainSize = 40, theme = "chalk")


# Custom exemple
amPie(data = data_pie)%>>%
  amOptions(main = "Social network", mainColor = "#FFFFFF", mainSize = 40,
            theme = "dark", legend = TRUE, legendPosition = "bottom",
             creditsPosition = "bottom-right" )
