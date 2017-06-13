

library(shiny)
library(rAmCharts)
library(jsonlite)
library(data.table)

data <- data.frame(date = seq(c(ISOdate(1999,12,31)), by = "5 min", length.out = 1000000), 
                   value = rnorm(1000000, 100, 50), value2 = rnorm(1000000, 100, 50))
attr(data[["date"]], "tzone") <- 'UTC'

data$date <- as.character(data$date)

# data <- data.frame(date = seq(c(ISOdate(1999,12,31)), by = "5 min", length.out = 100), 
#                    value = rnorm(100, 100, 50), value2 = rnorm(100, 100, 50))
# attr(data[["date"]], "tzone") <- 'UTC'
# 
# # data$date <- as.character(data$date)
# head(data)
# data[c(1, 4, 5, 6, 7), c(2, 3)] <- NA
# head(data)
# head(getAggregateTS(data, col_date = "date", ts = "10 min"))
# 
# data("data_stock_2")
# data_ts <- data_stock_2[1:100,]
# data_ts[c(1, 4, 5, 7), c(2, 3)] <- NA
# amTimeSeries(data_ts, "date", c("ts1", "ts2"), bullet = "round", bulletSize = 1)

