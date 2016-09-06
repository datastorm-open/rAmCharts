# 
# test an issue with the script fabric.js that causes inefficient exportation
# this error can only be reproduced with a package version built with amcharts v3.20.10
# 

library(rAmCharts)
library(magrittr)

data("data_pie")
amPie(data = data_pie, export = TRUE)

xlab <- 1:10
y <- rnorm(10)
color <- rep("black", 10)
color[c(1, 6, 9)] <- "#91c2d9"
dataProvider <- data.frame(xlab, y, color)

amSerialChart(categoryField = "xlab", dataProvider = dataProvider, precision = 2) %>%
  addGraph(valueField = "y", colorField = "color", bullet = "round", lineAlpha = 0) %>%
  setExport()