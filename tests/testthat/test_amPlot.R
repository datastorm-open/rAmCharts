context("amPlot")

library(pipeR)
library(data.table)
iris <- data.table(get("iris", "package:datasets"))

testthat::test_that("amPlot: x numeric, serial chart", {
  testthat::expect_silent({
    x <- rnorm(100)
    
    # Simple scatter plot with title and color
    # Also change type (set to "p" by default), avalaible "l", "sl", "st", "p", "b"
    amPlot(x = x, main = "Title", col = "lightblue", type = "b")
    
    # Change bullets, available "diamond", "square", "triangleUp", "triangleDown",
    # "triangleLeft", "triangleRight", "bubble", "yError"
    amPlot(x = x, bullet = "xError", error = sample(100))
    
    # Change lty and cex, ...
    amPlot(x = x, type = "b", lty = 1, cex = 10, lwd = 2)
  })
})

testthat::test_that("amPlot: x numeric, xy chart", {
  testthat::expect_silent({
    x <- sort(rnorm(100))
    y <- rnorm(100, sd = 10)
    weights <- rnorm(100)
    col <- factor(c(rep(1,25), rep(2,50), rep(3,25)))
    amPlot(x = x, y = y, type = "l", col = col, weights = weights, lty = 2, cex = 1, scrollbar = TRUE)
  })
})

testthat::test_that("amPlot: x character", {
  testthat::expect_silent({
    xc <- paste("cat.", 1:100)
    xf <- factor(xc)
    y <- rnorm(length(xc))
    amPlot(x = xc, y = y, type = 'p', cex = .01)
    
    # amPlot(x = xf, y = y, type = 'l') , x can be either a character or a factor
    
    # ---
    # x, y plot if x is a date (character)
    # ---
    start <- as.POSIXct('01-01-2015', format = '%d-%m-%Y')
    end <- as.POSIXct('31-12-2015', format = '%d-%m-%Y')
    date <- seq.POSIXt(from = start, to = end, by = 'day')
    date <- format(date, '%m-%d-%Y')
    
    y <- rnorm(length(date))
    amPlot(x = date, y = y, type = 'l', parseDates = TRUE, dataDateFormat = "MM-DD-YYYY")
  })
})

testthat::test_that("amPlot: data.frame", {
  testthat::expect_silent({   
    # Select the column to plot with a character or a numeric
    amPlot(iris, columns = "Sepal.Length")
    
    # Select several columns, a different type for each
    amPlot(iris, col = colnames(iris)[1:2], type = c("l", "st"), zoom = TRUE)
  })
})

testthat::test_that("amPlot: formula", {
  testthat::expect_silent({  
    amPlot(Petal.Length + Sepal.Length ~ Sepal.Width, data = iris, legend = TRUE, zoom = TRUE)
  })
})


testthat::test_that("amLines", {
  testthat::expect_silent({ 
    chart <- amPlot(x = rnorm(100), type = 'sl')
    chart %>>% amLines(x = rnorm(100), type = "p")
    
    
    chart <- chart %>>% amLines(x = rnorm(100), col = "blue")
    chart %>>%
      amLines(x = rnorm(100), type = "sl") %>>%
      amLines(x = rnorm(100), type = "p")
    
    # For an XY chart
    x <- sort(rnorm(100))
    y1 <- rnorm(100, sd = 10)
    y2 <- rnorm(100, sd = 10)
    y3 <- rnorm(100, sd = 10)
    chart <- amPlot(x = x, y = y1)
    amPlot(x = x, y = y1) %>>%
      amLines(x = y2, col = "blue") %>>%
      amLines(x = y3, type = "p")
  })
})