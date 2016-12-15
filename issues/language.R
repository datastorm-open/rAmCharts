# Change default English

## prepare data ---

start <- as.POSIXct(x = '01-01-2015', format = '%d-%m-%Y', tz = "UTC")
end <- as.POSIXct(x = '31-12-2015', format = '%d-%m-%Y', tz = "UTC")
date <- seq.POSIXt(from = start, to = end, by = 'day')
date <- format(date, '%m-%d-%Y')

## run chart ---

amPlot(x = date,
       y = runif(365),
       type = 'l',
       parseDates = TRUE,
       dataDateFormat = 'MM-DD-YYYY',
       language = "fr",
       export = TRUE)

