output$amlines0 <- rAmCharts::renderAmCharts({
##Data
data('AirP')
##Graph
amPlot(x = AirP$Period, y = AirP$AirPassengers, type='l')
})

output$code_amlines0 <- renderText({
  "
  ##Data
  data('AirP')
  ##Graph
  amPlot(x = AirP$Period, y = AirP$AirPassengers, type='l')
  "
})




output$amlines1 <- rAmCharts::renderAmCharts({
  ##Data
start <- as.POSIXct('01-01-2015', format = '%d-%m-%Y')
end <- as.POSIXct('31-12-2015', format = '%d-%m-%Y')
date <- seq.POSIXt(from = start, to = end, by = 'day')
date <- format(date, '%m-%d-%Y')
##Plot
amPlot(x = date, y = runif(365), type = 'l', parseDates = TRUE, dataDateFormat = 'MM-DD-YYYY')
})



output$code_amlines1 <- renderText({
  "
  ##Data
  start <- as.POSIXct('01-01-2015', format = '%d-%m-%Y')
  end <- as.POSIXct('31-12-2015', format = '%d-%m-%Y')
  date <- seq.POSIXt(from = start, to = end, by = 'day')
  date <- format(date, '%m-%d-%Y')
  ##Plot
  amPlot(x = date, y = runif(365), type = 'l', parseDates = TRUE, dataDateFormat = 'MM-DD-YYYY')
  "
})



output$amlines2 <- rAmCharts::renderAmCharts({
  ##Data
  start <- as.POSIXct('01-01-2015', format = '%d-%m-%Y')
  end <- as.POSIXct('11-04-2015', format = '%d-%m-%Y')
  date <- seq.POSIXt(from = start, to = end, by = 'day')
  date <- format(date, '%m-%d-%Y')
  ##Graph
  pipeR::pipeline(
  amPlot(y = rnorm(100), type = 'sl', x = date, parseDates = TRUE, dataDateFormat = 'MM-DD-YYYY'),
    amLines(y = rnorm(100), type = 'p'),
    setLegend(useGraphSettings = TRUE)
  )
})

output$code_amlines2 <- renderText({
  "
  ##Data
  start <- as.POSIXct('01-01-2015', format = '%d-%m-%Y')
  end <- as.POSIXct('11-04-2015', format = '%d-%m-%Y')
  date <- seq.POSIXt(from = start, to = end, by = 'day')
  date <- format(date, '%m-%d-%Y')
  ##Graph
  pipeR::pipeline(
    amPlot(y = rnorm(100), type = 'sl', x = date, parseDates = TRUE, dataDateFormat = 'MM-DD-YYYY'),
    amLines(y = rnorm(100), type = 'p'),
    setLegend(useGraphSettings = TRUE)
  )
  "
})
