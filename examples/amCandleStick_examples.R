# load data
data("data_candleStick2")

# Basic example
amCandlestick(data = data_candleStick2)

# change colors
amCandlestick(data = data_candleStick2, positiveColor = "black", negativeColor = "green")

# naming the axes
amCandlestick(data = data_candleStick2, xlab = "categories", ylab = "values")

# Rotate the labels for x axis
amCandlestick(data = data_candleStick2, labelRotation = 90)

# change names
amCandlestick(data = data_candleStick2, names = c("min", "begin", "end", "max"))

#Horizontal chart :
amCandlestick(data = data_candleStick2, horiz = TRUE)

#  Parse date              
amCandlestick(data = data_candleStick2, dataDateFormat = "YYYY-MM-DD")

# datas over months
data_candleStick2$category <- c("2015-01-01", "2015-02-01", "2015-03-01",
                                "2015-04-01", "2015-05-01", "2015-06-01",
                                "2015-07-01", "2015-08-01", "2015-09-01",
                                "2015-10-01", "2015-11-01", "2015-12-01")

amCandlestick(data = data_candleStick2, dataDateFormat = "YYYY-MM-DD", minPeriod = "MM")

# decimal precision
require(pipeR)
amCandlestick(data = data_candleStick2, horiz = TRUE) %>>%
  setProperties(precision = 2) 
