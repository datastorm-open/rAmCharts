# Basic example
set.seed(9)
category <- c("2015-01-01", "2015-01-02", "2015-01-03", "2015-01-04",
              "2015-01-05", "2015-01-06", "2015-01-07", "2015-01-08",
              "2015-01-09", "2015-01-10", "2015-01-11", "2015-01-12", "2015-01-20")
low <- rnorm(n = length(category) - 1, mean = 130, sd = 10)
open <- low + rnorm(n = length(category) - 1, mean = 4)
close <- open + rnorm(n = length(category) - 1, mean = 2)
high <- close + rnorm(n = length(category) - 1, mean = 4)

data_candle <- data.frame(category = category, open = c(open, 136.65),
                          close = c(close, 136.4), low = c(low, 134.45), high = c(high, 137.5),
                          stringsAsFactors = FALSE)

amCandlestick(data = data_candle)

# change colors
amCandlestick(data = data_candle, positiveColor = "black", negativeColor = "green")

# naming the axes
amCandlestick(data = data_candle, xlab = "values", ylab = "categories")

# Rotate the labels for x axis
amCandlestick(data = data_candle, labelRotation = 90)

# change names
amCandlestick(data = data_candle, names = c("min", "begin", "end", "max"))

#Horizontal chart :
amCandlestick(data = data_candle, dataDateFormat = "YYYY-MM-DD", horiz = TRUE)

#  Parse date              
amCandlestick(data = data_candle, dataDateFormat = "YYYY-MM-DD")

# datas over months
data_candle$category <- c("2015-01-01", "2015-02-01", "2015-03-01",
                          "2015-04-01", "2015-05-01", "2015-06-01",
                          "2015-07-01", "2015-08-01", "2015-09-01",
                          "2015-10-01", "2015-11-01", "2015-12-01", "2016-01-01")

amCandlestick(data = data_candle, dataDateFormat = "YYYY-MM-DD", minPeriod = "MM")

