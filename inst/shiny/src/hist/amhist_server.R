output$amhist1 <- renderAmCharts({
  amHist(x = rnorm(1000)*10)
})

output$code_amhist1 <- renderText({
  "
  amHist(x = rnorm(1000)*10)
  "
})

output$amhist2 <- renderAmCharts({
  amHist(x = rnorm(1000)*10, main = "Histogram", ylab = "y-axis", xlab = "x-axis", col = "red")
})

output$code_amhist2 <- renderText({
  "
  amHist(x = rnorm(1000)*10, main = 'Histogram', ylab = 'y-axis', xlab = 'x-axis', col = 'red')
  "
})

output$amhist3 <- renderAmCharts({
  amHist(x = rnorm(1000)*10, labels = TRUE, export = TRUE, freq = FALSE)
})

output$code_amhist3 <- renderText({
  "
  amHist(x = rnorm(1000)*10, labels = TRUE, export = TRUE)
  "
})