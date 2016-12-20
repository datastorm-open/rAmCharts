library(magrittr)

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

## example for a basic chart

df <- data.frame(
  country = c("USA","China","Japan","Germany","UK","France","India","Spain","Netherlands","Russia"),
  visits = c(3025,1882,1809,1322,1122,1114,984,711,665,580),
  color = c("#FF0F00","#FF6600","#FF9E01","#FCD202","#F1","#B0DE09","#04D215","#0D8ECF","#0D52D1","#2A0CD0")
)

# --- alternative 1

amSerialChart(responsive = TRUE, startDuration = 2, categoryField = "country", language = "fr") %>%
  setExport() %>%
  addGraph(balloonText = "<b>[[category]]: [[value]]</b>", fillColorsField = "color",
           fillAlphas = 0.85, lineAlpha = 0.1, type = "column", valueField = "visits") %>%
  setChartCursor(categoryBalloonEnabled = FALSE, cursorAlpha = 0, zoomable = FALSE) %>%
  setCategoryAxis(gridPosition = "start", axisAlpha = 0, gridAlpha = 0) %>%
  setDataProvider(df)

# --- alternative 2

amSerialChart(responsive = TRUE, startDuration = 2, categoryField = "country") %>%
  setExport() %>%
  setProperties(language = "fr") %>%
  addGraph(balloonText = "<b>[[category]]: [[value]]</b>", fillColorsField = "color",
           fillAlphas = 0.85, lineAlpha = 0.1, type = "column", valueField = "visits") %>%
  setChartCursor(categoryBalloonEnabled = FALSE, cursorAlpha = 0, zoomable = FALSE) %>%
  setCategoryAxis(gridPosition = "start", axisAlpha = 0, gridAlpha = 0) %>%
  setDataProvider(df)