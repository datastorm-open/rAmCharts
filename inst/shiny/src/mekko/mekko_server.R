 
output$mekko0 <- rAmCharts::renderAmCharts({
  ##Data
mekkodata <- data.frame(continent = c('North America', 'Asia', 'Europe'),
                         Trucks = c(40000, 90000, 30000),
                         SUVs = c(180000, 40000, 50000),
                         Cars = c(90000, 110000, 110000),
                         total = c(310000, 240000, 190000),
                         stringsAsFactors = FALSE)
##Plot
 pipeR::pipeline(
   amSerialChart(dataProvider = mekkodata, categoryField = 'continent'),
   setCategoryAxis( widthField = 'total', gridAlpha = 0.1, axisAlpha = 0),
   addValueAxis(stackType = '100% stacked', unit = '%', gridAlpha = 0.1, axisAlpha = 0),
   addGraph(title = 'Trucks', labelText = '[[value]]', valueField = 'Trucks', type = 'column', fillAlphas = 1),
   addGraph(title = 'SUVs', labelText = '[[value]]', valueField = 'SUVs', type = 'column', fillAlphas = 1),
   addGraph(title = 'Cars', labelText = '[[value]]', valueField = 'Cars', type = 'column', fillAlphas = 1),
  setLegend(useGraphSettings = TRUE)
 )
 
})

output$code_mekko0 <- renderText({
  "
  ##Data
  mekkodata <- data.frame(continent = c('North America', 'Asia', 'Europe'),
                          Trucks = c(40000, 90000, 30000),
                          SUVs = c(180000, 40000, 50000),
                          Cars = c(90000, 110000, 110000),
                          total = c(310000, 240000, 190000),
                          stringsAsFactors = FALSE)
  ##Plot
  pipeR::pipeline(
    amSerialChart(dataProvider = mekkodata, categoryField = 'continent'),
    setCategoryAxis( widthField = 'total', gridAlpha = 0.1, axisAlpha = 0),
    addValueAxis(stackType = '100% stacked', unit = '%', gridAlpha = 0.1, axisAlpha = 0),
    addGraph(title = 'Trucks', labelText = '[[value]]', valueField = 'Trucks', type = 'column', fillAlphas = 1),
    addGraph(title = 'SUVs', labelText = '[[value]]', valueField = 'SUVs', type = 'column', fillAlphas = 1),
    addGraph(title = 'Cars', labelText = '[[value]]', valueField = 'Cars', type = 'column', fillAlphas = 1),
    setLegend(useGraphSettings = TRUE)
  )
  "
})