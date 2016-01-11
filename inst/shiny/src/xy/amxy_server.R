


output$amxy0 <- rAmCharts::renderAmCharts({
  ##Data
  x <- sort(rnorm(100))
  y <- rnorm(100, sd = 10)
  ##Plot
  amPlot(x = x, y = y)
})



output$code_amxy0 <- renderText({
  "
  ##Data
  x <- sort(rnorm(100))
  y <- rnorm(100, sd = 10)
  ##Plot
  amPlot(x = x, y = y)
  "
})


output$amxy1 <- rAmCharts::renderAmCharts({
  ##Data
  x <- sort(rnorm(100))
  y <- rnorm(100, sd = 10)
  ##Plot
  amPlot(x = x, y = y, scrollbar = TRUE, hideYScrollbar = FALSE)
})

output$code_amxy1 <- renderText({
  "
  ##Data
  x <- sort(rnorm(100))
  y <- rnorm(100, sd = 10)
  ##Plot
  amPlot(x = x, y = y, scrollbar = TRUE, hideYScrollbar = FALSE)
  "
})



output$amxy2 <- rAmCharts::renderAmCharts({
  ##Data
  x <- sort(rnorm(100))
  y <- rnorm(100, sd = 10)
  ##Plot
  amPlot(x = x, y = y, type = 'p', weights = rnorm(100, sd = 5), main='My XY', col = 'blue')
})

output$code_amxy2 <- renderText({
  "
  ##Data
  x <- sort(rnorm(100))
  y <- rnorm(100, sd = 10)
  ##Plot
  amPlot(x = x, y = y, type = 'p', weights = rnorm(100, sd = 5), main='My XY', col = 'blue')
  "
})


output$amxy3 <- rAmCharts::renderAmCharts({
  ##Data
  x <- sort(rnorm(100))
  y1 <- rnorm(100, sd = 10)
  y2 <- rnorm(100, sd = 10)
  y3 <- rnorm(100, sd = 10)
  ##Plot
  pipeR::pipeline(
  amPlot(x = x, y = y1),
    amLines(x = y2, col = 'blue', type = 'p' ),
    amLines(x = y3, type = 'p')
  )
  
})

output$code_amxy3 <- renderText({
  "
  ##Data
  x <- sort(rnorm(100))
  y1 <- rnorm(100, sd = 10)
  y2 <- rnorm(100, sd = 10)
  y3 <- rnorm(100, sd = 10)
  ##Plot
  pipeR::pipeline(
    amPlot(x = x, y = y1),
    amLines(x = y2, col = 'blue', type = 'p' ),
    amLines(x = y3, type = 'p')
  )
  "
})






