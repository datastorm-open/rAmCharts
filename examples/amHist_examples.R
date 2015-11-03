x <- replicate(1000, {
  if (round(runif(1))) {
    rnorm(1)
  } else {
    rnorm(1, mean = 5)
  }
})

# Default method
(object <- amHist(x = x))
print(object)

# Without plot
amHist(x = x, plot = FALSE)

# With options
amHist(x = x, border = "blue")
amHist(x = x, col = "lightblue", breaks = 100)
amHist(x = x, col = "grey", breaks = 100)
amHist(x = x, col = "gray")
amHist(x = x, freq = FALSE)
amHist(x = x, breaks = "Scott")
amHist(x = x, breaks = "Scott", labels = TRUE)
amHist(x = x, breaks = "Scott", main = "Histogram", ylab = "y-axis", xlab = "x-axis", col = "red")
amHist(x = x, breaks = "Scott", main = "Histogram", 
       ylab = "y-axis", xlab = "x-axis", ylim = c(10, 15))

amHist(rnorm(100), breaks = "Scott", main = "Histogram", 
       ylab = "y-axis", xlab = "x-axis")

