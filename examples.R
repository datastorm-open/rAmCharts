# BASIC EXAMPLES ---
library(pipeR)
library(data.table)

### amPieChart
amPieChart(valueField = "value", titleField = "key", creditsPosition = "top-right",
           backgroundColor = "#ff0000"
) %>>% setDataProvider(data.frame(key = c("FR", "US"), value = c(20,10))
) %>>% setExport(position = "bottom-left") %>>% plot()

### amPieChart with listener
amPieChart(theme ="dark", valueField = "value", titleField = "key", creditsPosition = "top-right"
) %>>% setDataProvider(data.frame(key = c("FR", "US"), value = c(20,10))
) %>>% addListener("clickSlice" , "function(event){ alert('ok !'); }"
) %>>% plot()

### amPieChart with listener
legend <- amLegend(position = "right", marginRight = 100, autoMargins = FALSE, innerRadius = "30%"
) %>>% addListener("hideItem" , "function(event){alert('hide'); }")
amPieChart(theme ="dark", valueField = "value", titleField = "key", legend = legend
) %>>% setDataProvider(data.frame(key = c("FR", "US"), value = c(20,10))
) %>>% plot()



### amRadarChart
amRadarChart(theme = "chalk", startDuration = 1, categoryField = "attribute"
) %>>% setDataProvider (
  data.frame(attribute = c("data", "brand", "singleness"),
             p1 = c(.3, -1, 0), p2 = c(.7, 1, 2))
) %>>% addGraph(balloonText = "Utility : [[value]]", valueField = "p1", title = "p1"
) %>>% addGraph(balloonText = "Utility : [[value]]", valueField = "p2", title = "p2"
) %>>% addListener("clickGraphItem" , "function(event){ alert('ok !'); }"
) %>>% setLegend(useGraphSettings = TRUE) %>>% setExport %>>% plot

### amSerialChart
amSerialChart(theme = "dark", categoryField = "country", creditsPosition = "top-right"
) %>>% setDataProvider(data.frame(country = c("FR", "US"), visits = 1:2)
) %>>% addGraph(balloonText = "[[category]]: <b>[[value]]</b>", type = "column",
                valueField = "visits", fillAlphas = .8, lineAlpha = .2
) %>>% addListener("clickGraphItem" , "function(event){ alert('ok !'); }"
) %>>% setExport(position = "bottom-right"
) %>>% setChartCursor %>>% setChartScrollbar %>>% plot

### amXYChart
amXYChart(theme = "chalk", startDuration = 0.5, marginLeft = 46, marginBottom = 35
) %>>% setDataProvider(data.frame(
  y = c(10,5,-10,-6,15,13,1),  x = c(14,3,8,5,-4,1,6), value = c(59,50,19,65,92,8,16),
  y2 = c(-5,-15,-4,-5,-10,-2,0), x2 = c(-3,-8,6,-6,-8,0,-3), value2 = c(44,12,35,168,102,41,16))
) %>>% addValueAxes(position = "bottom", axisAlpha = 0) %>>%
  addValueAxes(minMaxMultiplier = 1.2, position = "left", axisAlpha = 0
  ) %>>% addGraph(balloonText = "x:<b>[[x]]</b> y:<b>[[y]]</b><br>value:<b>[[value]]</b>", bullet = "circle", bulletBorderAlpha = 0.2,
                  bulletAlpha = 0.8,lineAlpha=0, fillAlphas = 0, valueField = "value", xField = "x",yField = "y", maxBulletSize = 100
  ) %>>% addGraph(balloonText = "x:<b>[[x]]</b> y:<b>[[y]]</b><br>value:<b>[[value]]</b>",bullet="diamond",bulletBorderAlpha=0.2,
                  bulletAlpha = 0.8, lineAlpha = 0, fillAlphas = 0,valueField = "value2", xField = "x2", yField = "y2", maxBulletSize = 100
  ) %>>% setExport %>>% plot

###  AmGanttChart
ls <- list(data.frame(start = 7, duration = 2:3, task = c("Task #1", "Task #2")), 
           data.frame(start = 10, duration = 2:3, task = c("Task #1", "Task #2")))

amGanttChart(brightnessStep = 20, rotate = TRUE, theme = "dark",
             startField = "start", endField = "end", segmentsField = "segments", dataDateFormat = "YYYY-MM-DD", startDate = "2015-01-01",
             period = "hh", categoryField = "category", durationField = "duration",
             valueAxis = valueAxis(type = "date"), graph = amGraph(fillAlphas = 1)
) %>>% setDataProvider(data.frame(category = c("John", "Julia"))
) %>>% addSegment(1:2,  ls) %>>% setExport %>>% plot

### Chart with drilldown
amSerialChart(categoryField = "name"
) %>>% setDataProvider(data.frame(
  name = c("data", "Brand", "singleness"), start = c(8,10,6), end = c(11,13,10),
  color = c('#007FFF', "#007FFF", "#003FFF"), description = c("click to drill-down","",""))
) %>>% addGraph(valueField = "end", type = "column", openField = "start", lineAlpha = 0,
                fillColorsField = "color", fillAlphas = 0.9,
                balloonText = "<b>[[category]]</b><br>from [[start]] to [[end]]<br>[[description]]"
) %>>% addSubData(1, data.frame(modality = c("3G", "4G"), utility = c(-1,2), color = c("#007FFF", "#007FFF"))
) %>>% setSubChartProperties(type = "serial", creditsPosition = "bottom-right", categoryField = "modality") %>>%
  addGraph(valueField = 'utility', type = 'column', categoryField = "modality",
           lineAlpha = 0, fillColorsField = "color", fillAlphas = 0.9,
           balloonText = "[[modality]]: <b>[[utility]]</b>"
  ) %>>% plot

### gaugeChart
amAngularGaugeChart(theme = "chalk"
) %>>% addArrow(value = 100
) %>>% addAxe(gaugeAxis (bottomText = "0 km/h", endValue = 220, valueInterval = 10
) %>>% addBand(color = "#00CC00", endValue = 90, startValue = 0
) %>>% addBand(color = "#ffac29", endValue = 130, startValue = 90
) %>>% addBand(color = "#ea3838", endValue = 220, startValue = 130, innerRadius = "95%")
) %>>% setExport %>>% plot


### funnelChart
amFunnelChart(theme = "chalk", neckHeight = "30%", neckWidth = "40%", titleField = "title",
              valueField = "value", creditsPosition = "bottom-left", 
              dataProvider = data.frame(title = c("Website visits", "Downloads"),
                                        value = c(300, 123))
) %>>% plot


# ELABORATED EXAMPLES FROM THE API DEMONSTRATIONS ---

### Stacked Column Chart
amSerialChart(theme = "chalk",  categoryField = "year",  creditsPosition  =  "bottom-right"
) %>>% setDataProvider(data.frame( year  =  c(2003, 2004, 2005, 2006),  europe  =  c (2.5, 2.6, 2.8, 3.6), 
                                   namerica  =  c(2.5, 2.7, 2.9, 2.5),  asia  =   c(2.1, 2.2, 2.4, 2.2), 
                                   lamerica  =  c(0.3, 0.3, 0.3, 0.5),  meast  =  c(0.2, 0.3, 0.3, 0.2), 
                                   africa  =  c(0.1, 0.1, 0.1, 0.3) )
) %>>% setLegend(horizontalGap = 10, maxColumns = 1, position = "right", useGraphSettings = TRUE, markerSize = 10
) %>>% addValueAxes(stackType = "regular", axisAlpha = 0.3, gridAlpha = 0
) %>>% addGraph(balloonText = "<b>[[title]]</b><br><span style = 'font-size:14px'>[[category]]: <b>[[value]]</b></span>", 
                fillAlphas = 0.8, labelText = "[[value]]", lineAlpha = 0.3, 
                title = "Europe", type = "column", color = "#000000", valueField = "europe"
) %>>% addGraph(balloonText = "<b>[[title]]</b><br><span style = 'font-size:14px'>[[category]]: <b>[[value]]</b></span>", 
                fillAlphas = 0.8, labelText = "[[value]]", lineAlpha = 0.3, 
                title = "North America", type = "column", color = "#000000", valueField = "namerica"
) %>>% addGraph(balloonText = "<b>[[title]]</b><br><span style = 'font-size:14px'>[[category]]: <b>[[value]]</b></span>", 
                fillAlphas = 0.8, labelText = "[[value]]", lineAlpha = 0.3, 
                title = "Asia-Pacific", type = "column", color = "#000000", valueField = "asia"
) %>>% addGraph(balloonText = "<b>[[title]]</b><br><span style = 'font-size:14px'>[[category]]: <b>[[value]]</b></span>", 
                fillAlphas = 0.8, labelText = "[[value]]", lineAlpha = 0.3, 
                title = "Latin America", type = "column", color = "#000000", valueField = "lamerica"
) %>>% addGraph(balloonText = "<b>[[title]]</b><br><span style = 'font-size:14px'>[[category]]: <b>[[value]]</b></span>", 
                fillAlphas = 0.8, labelText = "[[value]]", lineAlpha = 0.3, 
                title = "Middle-East", type = "column", color = "#000000", valueField = "meast"
) %>>% addGraph(balloonText = "<b>[[title]]</b><br><span style = 'font-size:14px'>[[category]]: <b>[[value]]</b></span>", 
                fillAlphas = 0.8, labelText = "[[value]]", lineAlpha = 0.3, 
                title = "Africa", type = "column", color = "#000000", valueField = "africa"
) %>>% setCategoryAxis(gridPosition = "start", axisAlpha = 0, gridAlpha = 0, position = "left"
) %>>% setExport %>>% plot

### AmSerialChart
amSerialChart(usePeriod  =  "60mm", theme = "chalk", marginRight = 80, marginTop = 17,
              autoMarginOffset = 20, dataDateFormat = "YYYY-MM-DD JJ:NN:ss",
              categoryField = "date"
) %>>% setDataProvider(data.frame( price  =  runif(30),  date  =  as.character(seq(c(ISOdate(2015, 3, 28,  tz  = "CET")),  by  =  "hour",  length.out  =  30)) )
) %>>% addValueAxes(position  =  "left",  logarithmic  =  "true",  dashLength  =  1
) %>>% addGuide(dashLength  =  6, inside  =  TRUE, label  =  "average", lineAlpha  =  1,  value  =  0.3
) %>>% addGraph(bullet = "round", id = "g1", bulletBorderAlpha = 1, bulletColor = "#FFFFFF", bulletSize = 7, lineThickness = 2, title = "Price", 
                type = "smoothedLine", useLineColorForBulletBorder = TRUE, valueField = "price"
) %>>%  setChartScrollbar(chartScrollbar()
) %>>% setChartCursor(valueLineEnabled = TRUE, valueLineBalloonEnabled = TRUE, valueLineAlpha = 0.5, fullWidth = TRUE, cursorAlpha = 0.05, categoryBalloonDateFormat = "YYYY-MM-DD JJ:NN:ss"
) %>>% setCategoryAxis( parseDates = TRUE, minPeriod = "ss"
) %>>% setExport %>>% plot

### AmSerialChart: mix
# http://www.amcharts.com/demos/column-and-line-mix/
amSerialChart(addClassNames = TRUE, theme = "chalk", autoMargins = FALSE, marginLeft = 30,
              marginRight = 8, marginTop = 10, marginBottom = 26, startDuration = 1,
              categoryField = "year"
) %>>% setDataProvider(data.frame(year  =  c(2010, 2011, 2012, 2013, 2014), income  =  c(20.4, 20.6, 24.3, 21.5, 22.3), 
                                  expenses  =  c(31.5, 32.4, 25.6, 22.6, 24.9), dashLengthColumn = c(NA, NA, NA, NA, 5), 
                                  alpha = c(NA, NA, NA, NA, 0.2),  additional = c("", "", "", "", "(projection)"))
) %>>% setBalloon(adjustBorderColor = FALSE, horizontalPadding = 10, verticalPadding = 8, color = "#ffffff"
) %>>% addValueAxes(axisAlpha = 0, position = "left"
) %>>% addGraph(alphaField = "alpha", balloonText = "<span style = 'font-size:12px;'>[[title]] in [[category]]:<br><span style = 'font-size:20px;'>[[value]]</span> [[additional]]</span>", 
                fillAlphas = 1, title = "Income", type = "column", valueField = "income", dashLengthField = "dashLengthColumn"
) %>>% addGraph(id = "graph2", balloonText = "<span style = 'font-size:12px;'>[[title]] in [[category]]:<br><span style = 'font-size:20px;'>[[value]]</span> [[additional]]</span>", 
                bullet = "round", lineThickness = 3, bulletSize = 7, bulletBorderAlpha = 1, bulletColor = "#FFFFFF", useLineColorForBulletBorder = TRUE, bulletBorderThickness = 3, 
                fillAlphas = 0, lineAlpha = 1, title = "Expenses", valueField = "expenses"
) %>>% setCategoryAxis(gridPosition = "start", axisAlpha = 0, tickLength = 0
) %>>% setExport %>>% plot


### AmSerialChart: Column With Rotated Series
# http://www.amcharts.com/demos/exporting-chart-to-image/
amSerialChart(theme = "chalk", startDuration = 2, categoryField = "country", depth3D = 40,
              angle = 30
) %>>% setDataProvider(data.frame(
  country = c("USA", "China", "Japan", "Germany", "UK", "France", "India", "Spain", "Netherlands", "Russia"), 
  visits = c(3025, 1882, 1809, 1322, 1122, 1114, 984, 711, 665, 580), 
  color = c("#FF0F00", "#FF6600", "#FF9E01", "#FCD202", "#F8FF01", "#B0DE09", "#04D215", "#0D8ECF", "#0D52D1", "#2A0CD0"))
) %>>% addGraph(balloonText = "<b>[[category]]: [[value]]</b>", fillColorsField = "color", 
                fillAlphas = 0.85, lineAlpha = 0.1, type = "column", valueField = "visits"
) %>>% setChartCursor(categoryBalloonEnabled = FALSE, cursorAlpha = 0, zoomable = FALSE
) %>>% setCategoryAxis(gridPosition = "start", labelRotation = 45, axisAlpha = 0, gridAlpha = 0
) %>>% setExport %>>% plot

### AmSerialChart: stacked area
# http://www.amcharts.com/demos/stacked-area/
amSerialChart(theme = "dark", marginRight = 30, plotAreaBorderAlpha = 0,
              categoryField = "year"
) %>>% setDataProvider(data.frame(
  year  =  1994:2012, 
  cars  =  c(15, 13, 14, 15, 16, 18, 19, 22, 24, 20, 24, 25, 26, 35, 36, 37, 38, 39, 40), 
  motorcycles  =  c(15, 13, 14, 15, 16, 18, 19, 22, 24, 20, 24, 25, 26, 35, 36, 37, 38, 39, 40), 
  bicycles  =  c(15, 13, 14, 15, 16, 18, 19, 22, 24, 20, 24, 25, 26, 35, 36, 37, 38, 39, 40))
) %>>% setLegend(equalWidths = FALSE, periodValueText = "total: [[value.sum]]", position = "top", valueAlign = "left", valueWidth = 100
) %>>% addValueAxes(stackType = "regular", gridAlpha = 0.07, position = "left", title = "Traffic incidents"
) %>>% addGraph(balloonText = "<img src = 'http://www.amcharts.com/lib/3/images/car.png' style = 'vertical-align:bottom; margin-right: 10px; width:28px; height:21px;'><span style = 'font-size:14px; color:#000000;'><b>[[value]]</b></span>", 
                fillAlphas = 0.6, hidden = TRUE, lineAlpha = 0.4, title = "Cars", valueField = "cars"
) %>>% addGraph(balloonText = "<img src = 'http://www.amcharts.com/lib/3/images/motorcycle.png' style = 'vertical-align:bottom; margin-right: 10px; width:28px; height:21px;'><span style = 'font-size:14px; color:#000000;'><b>[[value]]</b></span>", 
                fillAlphas = 0.6, lineAlpha = 0.4, title = "Motorcycles", valueField = "motorcycles"
) %>>% addGraph(balloonText = "<img src = 'http://www.amcharts.com/lib/3/images/bicycle.png' style = 'vertical-align:bottom; margin-right: 10px; width:28px; height:21px;'><span style = 'font-size:14px; color:#000000;'><b>[[value]]</b></span>", 
                fillAlphas = 0.6, lineAlpha = 0.4, title = "Bicycles", valueField = "bicycles"
) %>>% setChartScrollbar(chartScrollbar()
) %>>% setChartCursor(cursorAlpha = 0
) %>>% setCategoryAxis(startOnAxis = TRUE, axisColor = "#DADADA", gridAlpha = 0.07
) %>>% setGuides(list(guide(category = "2001", toCategory = "2003", lineColor = "#CC0000", lineAlpha = 1, fillAlpha = 0.2, fillColor = "#CC0000", dashLength = 2, inside = TRUE, labelRotation = 90, label = "fines for speeding increased"), guide(category = "2007", lineColor = "#CC0000", lineAlpha = 1, dashLength = 2, inside = TRUE, labelRotation = 90, label =  "motorcycle fee introduced"))
) %>>% setExport %>>% plot


#### AmPieChart: simple radar
# http://www.amcharts.com/demos/radar-chart/
amPieChart(theme  =  "dark",  valueField = "litres", titleField = "country",
           balloonText = "[[title]]<br><span style = 'font-size:14px'><b>[[value]]</b> ([[percents]]%)</span>"
) %>>% setDataProvider(data.frame( country = c("Czech Republic", "Ireland", "Germany", "Australia", "Austria", "UK", "Belgium"), 
                                   litres = c(250, 130, 100, 50, 30, 10, 2) )
) %>>% setLegend(markerType = "circle", position = "right", marginRight = 80, autoMargins = FALSE
) %>>% setExport %>>% plot

### AmRadarChart: polar
# http://www.amcharts.com/demos/polar-chart/
amRadarChart(theme  =  "light",  startDuration  =  1, categoryField  =  "direction"
) %>>% setDataProvider(
  data.frame( direction  =  c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),  value  =  c(10, 7, 8, 9, 5, 4, 6, 9) )
) %>>% setValueAxes(list(valueAxis(gridType = "circles", minimum = 0, autoGridCount = FALSE, axisAlpha = 0.2, fillAlpha = 0.05, fillColor = "#FFFFFF", gridAlpha = 0.08, position = "left"))
) %>>% setGuides(list(guide(angle = 225, fillAlpha = 0.3, fillColor = "#0066CC", tickLength = 0, toAngle = 315, toValue = 14, value = 0, lineAlpha = 0), 
                      guide(angle = 45, fillAlpha = 0.3, fillColor = "#CC3333", tickLength = 0, toAngle = 135, toValue = 14, value = 0, lineAlpha = 0))
) %>>% addGraph(balloonText = "[[category]]: [[value]] m/s", bullet = "round", fillAlphas = 0.3, valueField = "value"
) %>>% setExport %>>% plot

### AmSerialChart: cylinder gauge
# http://www.amcharts.com/demos/cylinder-gauge/
amSerialChart(theme  =  "light", depth3D  =  100,  angle  =  30,  autoMargins  =  TRUE,
              categoryField  =  "category",  startDuration  =  2
) %>>% setDataProvider(data.frame( category  =  "Wine left in the barrel",  value1  =  30,  value2  =  70 )
) %>>% addValueAxes(stackType  =  "100%",  gridAlpha  =  0
) %>>% addGraph(type = "column", topRadius = 1, columnWidth = 1, showOnAxis = TRUE, lineThickness = 2, lineAlpha = 0.5, fillAlphas = 0.8, lineColor = "#FFFFFF", fillColors = "#8d003b", fillAlphas = 0.8, valueField = "value1"
) %>>% addGraph(type = "column", topRadius = 1, columnWidth = 1, showOnAxis = TRUE, lineThickness = 2, lineAlpha = 0.5, fillAlphas = 0.8, lineColor = "#cdcdcd", fillColors = "#cdcdcd", fillAlphas = 0.8, valueField = "value2"
) %>>% setCategoryAxis(axisAlpha = 0, labelOffset = 40, gridAlpha = 0
) %>>% setExport %>>% plot

#### AmSerialChart: Waterfall chart
# http://www.amcharts.com/demos/waterfall-chart/
amSerialChart(theme = "light", startDuration = 1, columnWidth = 0.6, categoryField = "name"
) %>>% setDataProvider( data.frame(
  name = c("Income A", "Income B", "Total Income", "Expenses A", "Expenses B", "Revenue"), 
  open  =  c(0, 11.13, 0, 12.92, 8.64, 0), 
  close =  c(11.13, 15.81, 15.81, 15.81, 12.92, 8.64), 
  color  =  c("#54cb6a", "#54cb6a", "#169b2f", "#cc4b48", "#cc4b48", "#1c8ceb"), 
  balloonValue  =  c(11.13, 4.68, 15.81, 2.89, 4.24, 11.13) )
) %>>% addValueAxes(axisAlpha = 0, gridAlpha = 0.1, position = "left"
) %>>% addGraph(balloonText = "<span style = 'color:[[color]]'>[[category]]</span><br><b>$[[balloonValue]] Mln</b>", 
                colorField = "color", fillAlphas = 0.8, labelText = "$[[balloonValue]]", lineColor = "#BBBBBB", openField = "open", type = "column", valueField = "close"
) %>>% addTrendLine(dashLength = 3, finalCategory = "Income B", finalValue = 11.13, initialCategory = "Income A", initialValue = 11.13, lineColor = "#888888"
) %>>% addTrendLine(dashLength = 3, finalCategory = "Expenses A", finalValue = 15.81, initialCategory = "Income B", initialValue = 15.81, lineColor = "#888888"
) %>>% addTrendLine(dashLength = 3, finalCategory = "Expenses A", finalValue = 15.81, initialCategory = "Income B", initialValue = 15.81, lineColor = "#888888"
) %>>% addTrendLine(dashLength = 3, finalCategory = "Expenses B", finalValue = 12.92, initialCategory = "Expenses A", initialValue = 12.92, lineColor = "#888888"
) %>>% addTrendLine(dashLength = 3, finalCategory = "Revenue", finalValue = 8.64, initialCategory = "Expenses B", initialValue = 8.64, lineColor = "#888888"
) %>>% setCategoryAxis(gridPosition = "start", axisAlpha = 0, gridAlpha = 0.1
) %>>% setExport %>>% plot

### amXYChart: bubble chart
# http://www.amcharts.com/demos/bubble-chart/
amXYChart(theme = "light", startDuration = 0.5, marginLeft = 46, marginBottom = 35
) %>>% setDataProvider(
  data.frame( y = c(10, 5, -10, -6, 15, 13, 1),  x = c(14, 3, 8, 5, -4, 1, 6), 
              value = c(59, 50, 19, 65, 92, 8, 16),  y2 = c(-5, -15, -4, -5, -10, -2, 0), 
              x2 = c(-3, -8, 6, -6, -8, 0, -3),  value2 = c(44, 12, 35, 168, 102, 41, 16) )
) %>>% setValueAxes(list(valueAxis(position = "bottom", axisAlpha = 0), valueAxis(minMaxMultiplier = 1.2, position = "left", axisAlpha = 0))
) %>>% addGraph(balloonText = "x:<b>[[x]]</b> y:<b>[[y]]</b><br>value:<b>[[value]]</b>", bullet = "circle", bulletBorderAlpha = 0.2, 
                bulletAlpha = 0.8, lineAlpha = 0, fillAlphas = 0, valueField = "value", xField = "x", yField = "y", maxBulletSize = 100
) %>>% addGraph(balloonText = "x:<b>[[x]]</b> y:<b>[[y]]</b><br>value:<b>[[value]]</b>", bullet = "diamond", bulletBorderAlpha = 0.2, 
                bulletAlpha = 0.8, lineAlpha = 0, fillAlphas = 0, valueField = "value2", xField = "x2", yField = "y2", maxBulletSize = 100
) %>>% setExport %>>% plot

### AmSerialChart: smoothed line chart
# http://www.amcharts.com/demos/smoothed-line-chart/
amSerialChart(theme = "dark", marginTop = 0, marginRight = 80, dataDateFormat = "YYYY",
              categoryField = "year"
) %>>% setDataProvider(
  data.frame(year = 1950:2015, value = runif(length(1950:2015), -1, 1))
) %>>% addValueAxes(axisAlpha = 0, position = "left"
) %>>% addGraph(id = "g1", balloonText =  "[[category]]<br><b><span style='font-size = 14px;'>[[value]]</span></b>",
                bullet = "round", bulletSize =  8, lineColor =  "#d1655d", lineThickness =  2,
                negativeLineColor =  "#637bb6", type =  "smoothedLine", valueField =  "value"
) %>>% setChartScrollbar(graph = "g1", gridAlpha = 0, color = "#888888", scrollbarHeight = 55, backgroundAlpha = 0,
                         selectedBackgroundAlpha = 0.1, selectedBackgroundColor = "#888888", graphFillAlpha = 0,
                         autoGridCount = TRUE, selectedGraphFillAlpha = 0, graphLineAlpha = 0.2,
                         graphLineColor = "#c2c2c2", selectedGraphLineColor = "#888888", selectedGraphLineAlpha = 1
) %>>% setChartCursor(categoryBalloonDateFormat = "YYYY", cursorAlpha = 0, valueLineEnabled =TRUE,
                      valueLineBalloonEnabled =TRUE, valueLineAlpha = 0.5, fullWidth = TRUE
) %>>% setCategoryAxis(minPeriod = "YYYY", parseDates = TRUE, minorGridAlpha = 0.1, minorGridEnabled = TRUE
) %>>% addListener("rendered", "function(event) { event.chart.zoomToIndexes(Math.round(event.chart.dataProvider.length * 0.4), Math.round(event.chart.dataProvider.length * 0.55)) }"
) %>>% setExport %>>% plot

### AmSerialChart
# http://www.amcharts.com/demos/candlestick-chart/
start <- as.POSIXct("01-01-2015", format = "%d-%m-%Y")
end <- as.POSIXct("31-12-2015", format = "%d-%m-%Y")
date <- seq.POSIXt(from = start, to = end, by = "day")
date <- format(date, "%m-%d-%Y")
low <- c() ; open <- c() ; close <- c() ; high <- c() ; median <- c()

n <- 100
invisible(
  sapply(1:length(date), function(i)
  {
    sample <- rnorm(n, mean = sample(1:10, 1), sd = sample(1:10/10, 1))
    quant <- boxplot(sample, plot = FALSE)$stats
    low <<- c(low, quant[1])
    open <<- c(open, quant[2])
    median <<- c(median, quant[3])
    close <<- c(close, quant[4])
    high <<- c(high, quant[5])
  })
)
dp <- data.table(date = date, low = round(low, 2), open = round(open, 2), 
                 close = round(close, 2), high = round(high, 2), median = round(median, 2)) 

amSerialChart(categoryField = "date", theme = "light",
              dataDateFormat = "MM-DD-YYYY", dataProvider = dp
) %>>% setCategoryAxis(parseDates = TRUE
) %>>% addValueAxes(position = "left"
) %>>% addGraph(id = "g1", type = "candlestick", lineColor = "#7f8da9",
                lowField = "low", closeField = "close",
                highField = "high", openField = "open", valueField = "median",
                balloonText = "Open : <b>[[open]]</b><br>Low =<b>[[low]]</b><br>High =<b>[[high]]</b><br>Close =<b>[[close]]</b><br>",
                lineColor = "#7f8da9", lineAlpha = 1, fillAlphas = 0.9, negativeBase = 5,
                negativeFillColors = "#db4c3c", negativeLineColor = "#db4c3c",
                title = "Price: "
) %>>% setChartCursor(valueLineEnabled = TRUE, valueLineBalloonEnabled = TRUE
) %>>% setChartScrollbar(graph = "g1", graphType = "line"
) %>>% addListener("rendered", "function(event) { event.chart.zoomToIndexes(event.chart.dataProvider.length - 10, event.chart.dataProvider.length - 1) }"
) %>>% setExport(position = "bottom-right") %>>% plot

### AmSerialChart: example of boxPlot
start <- as.POSIXct("01-01-2015", format = "%d-%m-%Y")
end <- as.POSIXct("31-12-2015", format = "%d-%m-%Y")
date <- seq.POSIXt(from = start, to = end, by = "day")
date <- format(date, "%m-%d-%Y %H")
n <- 100
low_outlier <- c() ; low <- c() ; open <- c()
close <- c() ; high <- c() ; high_outlier <- c() 
median <- c()
invisible(
  sapply(1:length(date), function(i)
  {
    sample <- rnorm(n, mean = sample(1:10, 1), sd = sample(1:10/10, 1))
    quant <- boxplot(sample, plot = FALSE)$stats
    low_outlier <<- c(low_outlier, quant[1])
    low <<- c(low, quant[1])
    open <<- c(open, quant[2])
    median <<- c(median, quant[3])
    close <<- c(close, quant[4])
    high <<- c(high, quant[5])
    high_outlier <<- c(high_outlier, quant[5])
  })
)
dp <- data.table(date = date, low_outlier = round(low_outlier, 2), low = round(low, 2),
                 open = round(open, 2), median = round(median, 2), close = round(close, 2),
                 high = round(high, 2), high_outlier = round(high_outlier, 2))


amSerialChart(categoryField = "date", theme = "light",
              dataDateFormat = "MM-DD-YYYY", dataProvider = dp
) %>>% setCategoryAxis(parseDates = TRUE
) %>>% addValueAxes(position = "left"
) %>>% addGraph(id = "g1", type = "candlestick",
                balloonText = "Low = <b>[[low_outlier]]</b><br/>1st quart. = <b>[[open]]</b><br/>Median = <b>[[median]]</b><br/>3rd quart. = <b>[[close]]</b><br/>High = <b>[[high_outlier]]</b><br/>",
                closeField = "close", fillColors = "#7f8da9", highField = "high",
                lineColor = "#7f8da9", lineAlpha = 1, lowField = "low",
                fillAlphas = "0.9",  negativeFillColors = "#db4c3c", negativeLineColor = "#db4c3c",
                openField = "open", title = "Price:", valueField = "close"
) %>>% addGraph(id = "g2", type = "step", valueField = "median",
                noStepRisers = TRUE, balloonText = ""
) %>>% addGraph(id = "g3", type = "step", valueField = "low_outlier",
                noStepRisers = TRUE, balloonText = ""
) %>>% addGraph(id = "g4", type = "step", valueField = "high_outlier",
                noStepRisers = TRUE, balloonText = ""
) %>>% setChartCursor(valueLineEnabled = TRUE, valueLineBalloonEnabled = TRUE
) %>>% setChartScrollbar(parseDates = TRUE
) %>>% addListener("rendered", "function(event) { event.chart.zoomToIndexes(event.chart.dataProvider.length - 10, event.chart.dataProvider.length - 1) }"
) %>>% setExport(position = "bottom-right") %>>% plot()


### AmStockChart
# http://www.amcharts.com/demos/multiple-data-sets/
firstDate <- Sys.Date()
chartData1 <- as.data.frame(t(sapply(0:20, FUN = function(i)
{
  date <- format(firstDate + i, "%m/%d/%Y")
  a <- round(runif(1) * (40 + i)) + 100 + i
  b <- round(runif(1) * (1000 + i)) + 500 + i * 2
  c(date = date, value = a,  volume = b)
})))

chartData2 <- as.data.frame(t(sapply(0:20, FUN = function(i)
{
  date <- format(firstDate + i, "%m/%d/%Y")
  a <- round(runif(1) * (100 + i)) + 200 + i
  b <- round(runif(1) * (1000 + i)) + 600 + i * 2
  c(date = date, value = a,  volume = b)
})))

chartData3 <- as.data.frame(t(sapply(0:20, FUN = function(i)
{
  date <- format(firstDate + i, "%m/%d/%Y")
  a <- round(runif(1) * (100 + i)) + 200 + i
  b <- round(runif(1) * (1000 + i)) + 600 + i * 2
  c(date = date, value = a,  volume = b)
})))

chartData4 <- as.data.frame(t(sapply(0:20, FUN = function(i)
{
  date <- format(firstDate + i, "%m/%d/%Y")
  a <- round(runif(1) * (100 + i)) + 200 + i
  b <- round(runif(1) * (1000 + i)) + 600 + i * 2
  c(date = date, value = a,  volume = b)
})))

amStockChart(theme = "light"
) %>>% addDataSet(dataSet(title = "first data set", categoryField = "date",
                          dataProvider = chartData1) %>>%
                    addFieldMapping(fromField = "value", toField = "value") %>>%
                    addFieldMapping(fromField = "volume", toField = "volume")
) %>>% addDataSet(dataSet(title = "second data set", categoryField = "date",
                          dataProvider = chartData2) %>>%
                    addFieldMapping(fromField = "value", toField = "value") %>>%
                    addFieldMapping(fromField = "volume", toField = "volume")
) %>>% addDataSet(dataSet(title = "third data set", categoryField = "date",
                          dataProvider = chartData3) %>>%
                    addFieldMapping(fromField = "value", toField = "value") %>>%
                    addFieldMapping(fromField = "volume", toField = "volume")
) %>>% addDataSet(dataSet(title = "fourth data set", categoryField = "date",
                          dataProvider = chartData4) %>>%
                    addFieldMapping(fromField = "value", toField = "value") %>>%
                    addFieldMapping(fromField = "volume", toField = "volume")
) %>>% addPanel(stockPanel(showCategoryAxis = FALSE, title = "Value", percentHeight = 70) %>>%
                  addStockGraph(id = "g1", valueField = "value", comparable = TRUE,
                                compareField = "value", balloonText = "[[title]] =<b>[[value]]</b>",
                                compareGraphBalloonText = "[[title]] =<b>[[value]]</b>"
                  ) %>>% setStockLegend(periodValueTextComparing = "[[percents.value.close]]%",
                                        periodValueTextRegular = "[[value.close]]")
) %>>% addPanel(stockPanel(title = "Volume", percentHeight = 30) %>>%
                  addStockGraph(valueField = "volume", type = "column",
                                fillAlphas = 1) %>>%
                  setStockLegend(periodValueTextRegular = "[[value.close]]")
) %>>% setChartScrollbarSettings(graph = "g1"
) %>>% setChartCursorSettings(valueBalloonsEnabled = TRUE, fullWidth = TRUE,
                              cursorAlpha = 0.1, valueLineBalloonEnabled = TRUE,
                              valueLineEnabled = TRUE, valueLineAlpha = 0.5
) %>>% setPeriodSelector(periodSelector(position = "left") %>>%
                           addPeriod(period = "MM", selected = TRUE, count = 1, label = "1 month") %>>%
                           addPeriod(period = "MAX", label = "MAX")
) %>>% setDataSetSelector(position = "left"
) %>>% setPanelsSettings(recalculateToPercents = FALSE
) %>>% plot()

### AmSerialChart: date
### Generate dates ----
library(data.table)
start <- as.POSIXct("01-01-2015", format = "%d-%m-%Y")
end <- as.POSIXct("31-12-2015", format = "%d-%m-%Y")
period <- seq.POSIXt(from = start, to = end, by = "10 min")
n <- length(period)

periodTemp <- seq.POSIXt(from = start, to = end, by = "3 hour")
nTemp <- length(periodTemp)

### Generate mesures ----
charge <- rnorm(n, mean = 500, sd= 200)
charge[ which(charge < 0) ] <- rnorm(length(which(charge < 0)), mean = 200, sd = 10)
temp <- rnorm(nTemp, mean = 15, sd = 10)
dtCharge <- data.table::data.table(charge, date = period)
setkey(dtCharge, date)
dtTemp <- data.table::data.table(temperature = temp, date = periodTemp)
setkey(dtTemp , date)
dp <- dtTemp[dtCharge]
dp[ , date := format(date, "%m-%d-%Y %H:%M:%S")]

nTest <- 1000
sample <- 1:nTest
amSerialChart(categoryField = "date", creditsPosition = "top-right",
              dataDateFormat = "MM-DD-YYYY JJ:NN:SS"
) %>>% setDataProvider(dataProvider = dp[ sample, ], keepNA = FALSE
) %>>% addGraph(id = "charge", valueField = "charge",
                valueAxis = "valueAxis1", lineColor = "#FF6600"
) %>>% addGraph(id = "temperature", type = "smoothedLine", valueField = "temperature",
                valueAxis = "valueAxis2", lineColor = "#B0DE09"
) %>>% addValueAxes(id = "valueAxis1", gridAlpha = 0, position = "right",
                    axisColor = "#FF6600", axisThickness = 2
) %>>% addValueAxes(id = "valueAxis2", gridAlpha = 0, position = "left",
                    axisColor = "#B0DE09", axisThickness = 2
) %>>% setCategoryAxis(parseDates = TRUE, minPeriod = "mm"
) %>>% setChartScrollbar %>>% setChartCursor  %>>% plot

# ----------------------
# Test AmStockChart ----
# ______________________

### Generate dates ----
start <- as.POSIXct("01-01-2015", format = "%d-%m-%Y")
end <- as.POSIXct("31-12-2015", format = "%d-%m-%Y")
period <- seq.POSIXt(from = start, to = end, by = "10 min")
n <- length(period)
periodTemp <- seq.POSIXt(from = start, to = end, by = "3 hour")
nTemp <- length(periodTemp)
### Generate mesures ----
charge <- rnorm(n, mean = 500, sd= 200)
charge[ which(charge < 0) ] <- rnorm(length(which(charge < 0)), mean = 200, sd = 10)
temp <- rnorm(nTemp, mean = 15, sd = 10)
dtCharge <- data.table::data.table(charge, date = period)
setkey(dtCharge, date)
dtTemp <- data.table::data.table(temperature = temp, date = periodTemp)
setkey(dtTemp , date)
dp <- dtTemp[dtCharge]
dp[ , date := format(date, "%m-%d-%Y %H:%M:%S")]

nTest <- 1000
sample <- 1:nTest
amStockChart(theme = "default" , dataDateFormat = "MM-DD-YYYY JJ:NN:SS"
) %>>% addDataSet(dataSet(title = "Courbe de charge", categoryField = "date") %>>%
                    setDataProvider(dp[ sample, ], keepNA = FALSE) %>>%
                    addFieldMapping(fromField = "charge", toField = "charge") %>>%
                    addFieldMapping(fromField = "temperature", toField = "temperature") %>>%
                    addStockEvent( date = "01-02-2015 23:00:00", type = "sign", graph ="g1",
                                   text = "I am a stockEvent", description = "I am a property of a DataSet")
) %>>% addPanel(stockPanel(showCategoryAxis = FALSE, title = "Charge", percentHeight = 70) %>>%
                  addStockGraph(id = "g1", valueField = "charge", comparable = TRUE,
                                compareField = "charge", balloonText = "[[title]] =<b>[[value]]</b>",
                                compareGraphBalloonText = "[[title]] =<b>[[value]]</b>"
                  ) %>>% setStockLegend(periodValueTextComparing = "[[percents.value.close]]%",
                                        periodValueTextRegular = "[[value.close]]")
) %>>% addPanel(stockPanel(title = "Temperature", percentHeight = 30) %>>%
                  addStockGraph(valueField = "temperature", fillAlphas = 1) %>>%
                  setStockLegend(periodValueTextRegular = "[[value.close]]")
) %>>% setChartScrollbarSettings(graph = "g1"
) %>>% setChartCursorSettings(valueBalloonsEnabled = TRUE, fullWidth = TRUE,
                              cursorAlpha = 0.1, valueLineBalloonEnabled = TRUE,
                              valueLineEnabled = TRUE, valueLineAlpha = 0.5
) %>>% setPeriodSelector(periodSelector(position = "left") %>>%
                           addPeriod(period = "MM", selected = TRUE, count = 1, label = "1 month") %>>%
                           addPeriod(period = "MAX", label = "MAX")
) %>>% setDataSetSelector(position = "left"
) %>>% setCategoryAxesSettings(minPeriod = "ss"
) %>>% setPanelsSettings(recalculateToPercents = FALSE
) %>>% plot(width = "100%")


