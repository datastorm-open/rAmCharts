require(pipeR)

# For a simple chart
amPlot(x = rnorm(100), type = 'sl') %>>%
  amLines(x = rnorm(100), type = "p")

amPlot(x = rnorm(100), type = 'sl') %>>%
  amLines(x = rnorm(100), col = "blue") %>>%
  amLines(x = rnorm(100), type = "sl") %>>%
  amLines(x = rnorm(100), type = "p")

# For an XY chart
x <- sort(rnorm(100))
y1 <- rnorm(100, sd = 10)
y2 <- rnorm(100, sd = 10)
y3 <- rnorm(100, sd = 10)
amPlot(x = x, y = y1) %>>%
  amLines(x = y2, col = "blue") %>>%
  amLines(x = y3, type = "p")
