library(pipeR)
library(rAmCharts)

# ----------------
# EXAMPLES NUMERIC
# ----------------

# ---
# x, y plot
# ---

x <- sort(rnorm(100))
y <- rnorm(100, sd = 10)

# change type
amPlot(x = x, y = y) # default type = "p"
amPlot(x = x, y = y, type = "sl")
amPlot(x = x, y = y, type = "l")

# add a scrollbar
amPlot(x = x, y = y, scrollbar = TRUE)
amPlot(x = x, y = y, scrollbar = TRUE, hideYScrollbar = FALSE)

# change lty
amPlot(x = x, y = y, type = "l", lty = 1)

# add weight
weights <- rnorm(100, sd = 5)
amPlot(x = x, y = y, type = "p", weights = weights)

# change cex
amPlot(x = x, y = y, type = "p", cex = 1)
amPlot(x = x, y = y, type = "p", cex = 50)

# add title
amPlot(x = rnorm(100), y = rnorm(100), main = "title")

# ---
# x plot
# ---

amPlot(x = rnorm(100))

# add title
amPlot(x = rnorm(100), main = "Title")

# set a title
amPlot(x = rnorm(100), main = "Title")

# provide only x
amPlot(x = rnorm(100))

# change type (set to "l" by default)
x <- rnorm(100)
amPlot(x = rnorm(100, mean = 10))
amPlot(x = x, type = "sl")
amPlot(x = x, type = "st")
amPlot(x = x, type = "p")
amPlot(x = x, type = "b")

# change color
amPlot(x = rnorm(100), col = "lightblue")

# disable the cursor
amPlot(x = rnorm(100), cursor = FALSE)

# allow scrollbar
amPlot(x = rnorm(100), scrollbar = TRUE)

# change bullets
x <- rnorm(100)
amPlot(x = x, bullet = "diamond")
amPlot(x = x, bullet = "square")
amPlot(x = x, bullet = "triangleUp")
amPlot(x = x, bullet = "triangleDown")
amPlot(x = x, bullet = "triangleLeft")
amPlot(x = x, bullet = "triangleRight")
amPlot(x = x, bullet = "bubble")
amPlot(x = x, bullet = "yError", error = sample(100))
amPlot(x = x, bullet = "xError", error = sample(100))

# change lty
x <- rnorm(100)
amPlot(x = x, type = "b", lty = 1)
amPlot(x = x, type = "b", lty = 6)

# change cex
x <- rnorm(100)
amPlot(x = x, cex = 1)
amPlot(x = x, cex = 10)

# change lwd
x <- rnorm(100)
amPlot(x = x, type = "st", lwd = 1)
amPlot(x = x, type = "st", lwd = 2)

# -------------------
# EXAMPLES DATA.FRAME
# -------------------

iris <- data.table(get("iris", "package:datasets"))
amPlot(iris, columns = "Sepal.Length")

amPlot(iris, col = colnames(iris)[1:2], type = c("l", "st"))
amPlot(iris)

co2 <- data.table(get("CO2", "package:datasets"))
amPlot(co2)
