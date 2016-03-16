library(pipeR)
library(data.table)

# ----------------
# EXAMPLES NUMERIC
# ----------------

# ---
# x, y plot if x is a factor or a character
# ---


xc <- paste("cat.", 1:100)
xf <- factor(xc)
y <- rnorm(length(xc))

# Change type...
amPlot(x = xc, y = y, type = 'l')
# amPlot(x = xf, y = y, type = 'l') , x can be either a character or a factor

# ---
# x, y plot if x is a date (character)
# ---
start <- as.POSIXct('01-01-2015', format = '%d-%m-%Y')
end <- as.POSIXct('31-12-2015', format = '%d-%m-%Y')
date <- seq.POSIXt(from = start, to = end, by = 'day')
date <- format(date, '%m-%d-%Y')

y <- rnorm(length(date))
amPlot(x = date, y = y, type = 'l') # default parseDates = FALSE

amPlot(x = date, y = y, type = 'l', parseDates = TRUE, dataDateFormat = "MM-DD-YYYY")


# ---
# x, y plot if both x, y are numeric
# ---

x <- sort(rnorm(100))
y <- rnorm(100, sd = 10)

# Add a legend, and scrollbar
amPlot(x = x, y = y, type = "l", legend = TRUE, scrollbar = TRUE)

# Change lty
amPlot(x = x, y = y, type = "l", lty = 2)

# Add weight
weights <- rnorm(100, sd = 5)
amPlot(x = x, y = y, type = "p", weights = weights)

# Change cex and color
amPlot(x = x, y = y, type = "p", cex = 10, col = "blue", main = "title")

col <- factor(c(rep(1,25), rep(2,50), rep(3,25)))
amPlot(x = x, y = y, col = col)

# ---
# x plot
# ---
x <- rnorm(100)

# Simple scatter plot with title and color
amPlot(x = x, main = "Title", col = "lightblue", main = "My title")

# Change type (set to "p" by default), avalaible "l", "sl", "st", "p", "b"
amPlot(x = x, type = "b")

# Change bullets, available "diamond", "square", "triangleUp", "triangleDown",
# "triangleLeft", "triangleRight", "bubble", "yError"
amPlot(x = x, bullet = "xError", error = sample(100))

# Change lty and cex, ...
amPlot(x = x, type = "b", lty = 1, cex = 10, lwd = 2)

# -------------------
# EXAMPLES DATA.FRAME
# -------------------

iris <- get("iris", "package:datasets")

# Select all numeric columns
amPlot(iris)

# Select the column to plot with a character or a numeric
amPlot(iris, columns = "Sepal.Length")
amPlot(iris, columns = 1)

# Select several columns
amPlot(iris, col = colnames(iris)[1:2], type = c("l", "st"), zoom = TRUE)

# Add parameter from amOptions
co2 <- data.table(get("CO2", "package:datasets"))
amPlot(co2, zoom = TRUE)

# -------------------
# ADD OTHER SERIE(S)
# -------------------

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


# -------------------
# FORMULA
# -------------------
iris <- data.table(get("iris", "package:datasets"))

amPlot(x = iris$Sepal.Length, y = iris$Sepal.Width, xlab = "Sepal.Length")
amPlot(Sepal.Length~Sepal.Width, data = iris)
amPlot(Petal.Length + Sepal.Length ~ Sepal.Width, data = iris) %>>%
  setLegend(useGraphSettings = TRUE)

# Add parameter from amOptions
amPlot(Sepal.Length~Sepal.Width, data = iris, zoom = TRUE)
