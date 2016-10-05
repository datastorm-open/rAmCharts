library(rAmCharts)
library(magrittr)

y1 <- rnorm(150)
y2 <- rnorm(150)
chart <- amPlot(y1) %>% amLines(y2)
chart %>% amLines(y2)

chart_widget <- plot(chart)
chart_widget %>% onRender("function(el, x, data) {console.l}", data = 1)

xlab <- paste0("Value", 1:100)
y1 <- sample(c(rnorm(100), rep(NA, 50)))
y2 <- rnorm(100)
dataProvider <- data.frame(xlab = xlab, y1 = y1, y2 = y2)

amSerialChart(categoryField = "xlab", precision = 2) %>%
  setDataProvider(dataProvider = dataProvider, keepNA = TRUE) %>%
  addGraph(valueField = "y1", lineColor = "darkblue") %>%
  addGraph(valueField = "y2", lineColor = "grey") %>%
  setChartCursor()

xlab <- 1:150
y <- rnorm(150)
color <- numeric(150L)
color[c(100, 110, 125)] <- "red"
dataProvider <- data.frame(xlab, y, color)

amSerialChart(categoryField = "xlab", dataProvider = dataProvider, precision = 2) %>%
  addGraph(valueField = "y", colorField = "color", bullet = "round", lineAlpha = 0) %>%
  setExport()

  