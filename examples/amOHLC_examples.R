require(pipeR)

# load data
data("data_candleStick2")

# Basic example
amOHLC(data = data_candleStick2)

# change colors
amOHLC(data = data_candleStick2, positiveColor = "green", negativeColor = "red")

# naming the axes
amOHLC(data = data_candleStick2, xlab = "categories", ylab = "values") %>>% setChartCursor()

# Rotate the labels for x axis
amOHLC(data = data_candleStick2, labelRotation = 90)

# change names
amOHLC(data = data_candleStick2, names = c("min", "begin", "end", "max")) %>>% setChartCursor()

# use amOptions
amOHLC(data = data_candleStick2, zoom = FALSE)
