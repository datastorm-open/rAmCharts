

library(shiny)
library(rAmCharts)
library(jsonlite)
library(data.table)


data <- data.frame(date = seq(c(ISOdate(1999,12,31)), by = "5 min", length.out = 1000000), 
                   value = rnorm(1000000, 100, 50), value2 = rnorm(1000000, 100, 50))
attr(data[["date"]], "tzone") <- 'UTC'

data$date <- as.character(data$date)