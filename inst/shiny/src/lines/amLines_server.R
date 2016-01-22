output$amlines0 <- rAmCharts::renderAmCharts({
  # load data
  data('data_AirPassengers')
  # plot
  amPlot(AirPassengers ~ Period, data = data_AirPassengers, type = 'l')
})

output$code_amlines0 <- renderText({
  "
  # load data
  data('data_AirPassengers')
  # plot
  amPlot(AirPassengers ~ Period, data = data_AirPassengers, type = 'l')
  "
})

# ---

output$amlines1 <- rAmCharts::renderAmCharts({
  # load data
  start <- as.POSIXct('01-01-2015', format = '%d-%m-%Y')
  end <- as.POSIXct('31-12-2015', format = '%d-%m-%Y')
  date <- seq.POSIXt(from = start, to = end, by = 'day')
  date <- format(date, '%m-%d-%Y')
  # plot
  amPlot(x = date, y = runif(365), type = 'l', parseDates = TRUE, dataDateFormat = 'MM-DD-YYYY')
})

output$code_amlines1 <- renderText({
  "
  # load data
  start <- as.POSIXct('01-01-2015', format = '%d-%m-%Y')
  end <- as.POSIXct('31-12-2015', format = '%d-%m-%Y')
  date <- seq.POSIXt(from = start, to = end, by = 'day')
  date <- format(date, '%m-%d-%Y')
  # plot
  amPlot(x = date, y = runif(365), type = 'l', parseDates = TRUE, dataDateFormat = 'MM-DD-YYYY')
  "
})

# ---

output$amlines2 <- rAmCharts::renderAmCharts({
  # prepare data
  start <- as.POSIXct('01-01-2015', format = '%d-%m-%Y')
  end <- as.POSIXct('11-04-2015', format = '%d-%m-%Y')
  date <- seq.POSIXt(from = start, to = end, by = 'day')
  date <- format(date, '%m-%d-%Y')
  # plot
  pipeR::pipeline(
    amPlot(y = rnorm(100), type = 'sl', x = date, parseDates = TRUE, dataDateFormat = 'MM-DD-YYYY'),
    amLines(y = rnorm(100), type = 'p'),
    setLegend(useGraphSettings = TRUE)
  )
})

output$code_amlines2 <- renderText({
  "
  # prepare data
  start <- as.POSIXct('01-01-2015', format = '%d-%m-%Y')
  end <- as.POSIXct('11-04-2015', format = '%d-%m-%Y')
  date <- seq.POSIXt(from = start, to = end, by = 'day')
  date <- format(date, '%m-%d-%Y')
  # plot
  pipeR::pipeline(
    amPlot(y = rnorm(100), type = 'sl', x = date, parseDates = TRUE, dataDateFormat = 'MM-DD-YYYY'),
    amLines(y = rnorm(100), type = 'p'),
    setLegend(useGraphSettings = TRUE)
  )
  "
})
