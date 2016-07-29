library(rAmCharts)
library(magrittr)

y1 <- rnorm(100)
y2 <- rnorm(150)
chart <- amPlot(y1) %>% amLines(y2)
chart %>% amLines(y2)

xlab <- paste0("Value", 1:150)
y1 <- sample(c(rnorm(100), rep(NA, 50)))
y2 <- rnorm(150)
dataProvider <- data.frame(xlab = xlab, y1 = y1, y2 = y2)

amSerialChart(categoryField = "xlab", precision = 2) %>%
  setDataProvider(dataProvider = dataProvider, keepNA = TRUE) %>%
  addGraph(valueField = "y1", lineColor = "darkblue") %>%
  addGraph(valueField = "y2", lineColor = "grey") %>%
  setChartCursor()

