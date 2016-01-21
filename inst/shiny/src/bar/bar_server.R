
output$bar0 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_bar')
  ##Plot
  pipeR::pipeline(
    amSerialChart(),
    setDataProvider(data_bar),
    setProperties(type='serial', categoryField='country'),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',type='column',valueField='visits')
  )
})



output$code_bar0 <- renderText({
  "
  ##Data
  data('data_bar')
  ##Plot
  pipeR::pipeline(
    amSerialChart(),
    setDataProvider(data_bar),
    setProperties(type='serial', categoryField='country'),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',type='column',valueField='visits')
  )
  "
})





output$bar1 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_bar')
  ##Plot
  pipeR::pipeline(
    amSerialChart(),
    setDataProvider(data_bar),
    setProperties(type='serial', categoryField='country'),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',type='column',valueField='visits',fillColorsField = 'color',
             fillAlphas = 1, lineAlpha=0),
    setCategoryAxis(labelRotation = 45),
    addTitle(text = 'My bar')
  )
})



output$code_bar1 <- renderText({
  "
  ##Data
  data('data_bar')
  ##Plot
  pipeR::pipeline(
    amSerialChart(),
    setDataProvider(data_bar),
    setProperties(type='serial', categoryField='country'),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',type='column',valueField='visits',fillColorsField = 'color',
      fillAlphas = 1, lineAlpha=0),
    setCategoryAxis(labelRotation = 45),
    addTitle(text = 'My bar')
  )
  "
})


output$bar2 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_gbar')
  ##Plot
  pipeR::pipeline(
    amSerialChart(),
    setDataProvider(data_gbar),
    setProperties(type='serial', categoryField='year'),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',
             type='column',valueField='income', fillAlphas = 1, lineAlpha=0),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',
             type='column',valueField='expenses', fillAlphas = 1, lineAlpha=0)
  )
})


output$code_bar2 <- renderText({
  "
  ##Data
 data('data_gbar')
  ##Plot
  pipeR::pipeline(
    amSerialChart(),
    setDataProvider(data_gbar),
    setProperties(type='serial', categoryField='year'),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',type='column',valueField='income', fillAlphas = 1, lineAlpha=0),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',type='column',valueField='expenses', fillAlphas = 1, lineAlpha=0)
  )
  "
})



output$bar3 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_gbar')
  ##Plot
  pipeR::pipeline(
    amSerialChart(),
    setDataProvider(data_gbar),
    setProperties(type='serial', categoryField='year'),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',type='column',
             valueField='income', fillAlphas = 1, lineAlpha=0, title = 'income'),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',type='column',
             valueField='expenses', fillAlphas = 1, lineAlpha=0, title = 'expenses'),
    addValueAxes(stackType = 'regular'),
    setChartCursor(),
    setLegend(position = 'bottom' ,useGraphSettings = TRUE, periodValueText = 'total: [[value.sum]]', valueWidth = 100)
  )
})





output$code_bar3 <- renderText({
  "
 ##Data
 data('data_gbar')
  ##Plot
  pipeR::pipeline(
    amSerialChart(),
    setDataProvider(data_gbar),
    setProperties(type='serial', categoryField='year'),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',type='column',valueField='income', fillAlphas = 1, lineAlpha=0, title = 'income'),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',type='column',valueField='expenses', fillAlphas = 1, lineAlpha=0, title = 'expenses'),
    addValueAxes(stackType = 'regular'),
    setChartCursor(),
    setLegend(position = 'bottom' ,useGraphSettings = TRUE, periodValueText = 'total: [[value.sum]]', valueWidth = 100)
  )
  "
})



output$bar5 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_gbar')
  ##Plot
  pipeR::pipeline(
    amSerialChart(),
    setDataProvider(data_gbar),
    setProperties(type='serial', categoryField='year', 
                  depth3D = 60, angle = 30, export = list(enabled = TRUE)),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',
             type='column',valueField='expenses', fillAlphas = 1, lineAlpha=0, title = 'expenses'),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',
             type='column',valueField='income', fillAlphas = 1, lineAlpha=0, title = 'income'),
    setChartCursor(),
    setLegend(position = 'bottom' ,useGraphSettings = TRUE, periodValueText = 'total: [[value.sum]]', valueWidth = 100),
 addValueAxes(stackType = '3d')
  )
})

output$code_bar5 <- renderText({
  "
  ##Data
 data('data_gbar')
  ##Plot
  pipeR::pipeline(
    amSerialChart(),
    setDataProvider(data_gbar),
    setProperties(type='serial', categoryField='year', depth3D = 60, angle = 30, export = list(enabled = TRUE))),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',type='column',valueField='expenses', fillAlphas = 1, lineAlpha=0, title = 'expenses'),
    addGraph(balloonText='<b>[[category]]: [[value]]</b>',type='column',valueField='income', fillAlphas = 1, lineAlpha=0, title = 'income'),
    setChartCursor(),
    setLegend(position = 'bottom' ,useGraphSettings = TRUE, periodValueText = 'total: [[value.sum]]', valueWidth = 100),
    addValueAxes(stackType = '3d')
  )
  "
})

















output$bar4 <- rAmCharts::renderAmCharts({
    ##Data
  df <- data.frame(
    name=c('Income A','Income B','Total Income','Expenses A','Expenses B','Revenue'),
    open = c(0,11.13,0,12.92,8.64,0),
    close= c(11.13,15.81,15.81,15.81,12.92,8.64),
    color = c('#54cb6a','#54cb6a','#169b2f','#cc4b48','#cc4b48','#1c8ceb'),
    balloonValue = c(11.13,4.68,15.81,2.89,4.24,11.13)
  )
  
  ##Plot
  pipeR::pipeline(
  amSerialChart(),
    setDataProvider(df),
    setProperties(theme='light',type='serial',startDuration=1,columnWidth=0.6,categoryField='name'),
    addValueAxis(axisAlpha=0,gridAlpha=0.1,position='left'),
    addGraph(balloonText='[[category]]</span><br><b>$[[balloonValue]] Mln</b>',
             colorField='color',fillAlphas=0.8,
             labelText='$[[balloonValue]]',lineColor='#BBBBBB',openField='open',type='column',valueField='close'),
    addTrendLine(dashLength=3,finalCategory='Income B',
                 finalValue=11.13,initialCategory='Income A',initialValue=11.13,lineColor='#888888'),
    addTrendLine(dashLength=3,finalCategory='Expenses A',
                 finalValue=15.81,initialCategory='Income B',initialValue=15.81,lineColor='#888888'),
    addTrendLine(dashLength=3,finalCategory='Expenses A',
                 finalValue=15.81,initialCategory='Income B',initialValue=15.81,lineColor='#888888'),
    addTrendLine(dashLength=3,finalCategory='Expenses B',
                 finalValue=12.92,initialCategory='Expenses A',initialValue=12.92,lineColor='#888888'),
    addTrendLine(dashLength=3,finalCategory='Revenue',
                 finalValue=8.64,initialCategory='Expenses B',initialValue=8.64,lineColor='#888888'),
    setCategoryAxis(gridPosition='start',axisAlpha=0,gridAlpha=0.1)
  )
})



output$code_bar4 <- renderText({
  "
  ##Data
  df <- data.frame(
    name=c('Income A','Income B','Total Income','Expenses A','Expenses B','Revenue'),
    open = c(0,11.13,0,12.92,8.64,0),
    close= c(11.13,15.81,15.81,15.81,12.92,8.64),
    color = c('#54cb6a','#54cb6a','#169b2f','#cc4b48','#cc4b48','#1c8ceb'),
    balloonValue = c(11.13,4.68,15.81,2.89,4.24,11.13)
  )

  ##Plot
  pipeR::pipeline(
    amSerialChart(),
    setDataProvider(df),
    setProperties(theme='light',type='serial',startDuration=1,columnWidth=0.6,categoryField='name'),
    addValueAxis(axisAlpha=0,gridAlpha=0.1,position='left'),
    addGraph(balloonText='[[category]]</span><br><b>$[[balloonValue]] Mln</b>',
    colorField='color',fillAlphas=0.8,labelText='$[[balloonValue]]',lineColor='#BBBBBB',openField='open',type='column',valueField='close'),
    addTrendLine(dashLength=3,finalCategory='Income B',finalValue=11.13,initialCategory='Income A',initialValue=11.13,lineColor='#888888'),
    addTrendLine(dashLength=3,finalCategory='Expenses A',finalValue=15.81,initialCategory='Income B',initialValue=15.81,lineColor='#888888'),
    addTrendLine(dashLength=3,finalCategory='Expenses A',finalValue=15.81,initialCategory='Income B',initialValue=15.81,lineColor='#888888'),
    addTrendLine(dashLength=3,finalCategory='Expenses B',finalValue=12.92,initialCategory='Expenses A',initialValue=12.92,lineColor='#888888'),
    addTrendLine(dashLength=3,finalCategory='Revenue',finalValue=8.64,initialCategory='Expenses B',initialValue=8.64,lineColor='#888888'),
    setCategoryAxis(gridPosition='start',axisAlpha=0,gridAlpha=0.1)
  )
  "
})



output$bar6 <- rAmCharts::renderAmCharts({
  ##Data
  n <- 15
  (male <- round(sort(runif(n)), 1))
  (female <- -round(sort(runif(n)), 1))
  (category <- factor(paste('cat.', 1:15)))
  dataProvider <- data.table(male, female, category)
  labelFunction1 <- htmlwidgets::JS('function(item) {',
                                    'return Math.abs(item.values.value);',
                                    '}')
  balloonFunction <- htmlwidgets::JS('function(item) {',
                                     'return item.category + \': \' + Math.abs(item.values.value) + \'%\';',
                                     '}')
  labelFunction2 <- htmlwidgets::JS('function(value) {',
                                    'return Math.abs(value) + \'%\';',
                                    '}')
  
  valueAxis_obj <- valueAxis(gridAlpha = 0, ignoreAxisWidth = TRUE, id = 'axis1',
                             labelFunction = labelFunction2)

##Graph
  pipeline(
    amSerialChart(startDuration = 0, rotate = TRUE, marginBottom = 50,
                  categoryField = 'category'),
    addGraph(fillAlphas = .8, lineAlpha = .2, type = 'column', valueField = 'male',
             title = 'Male', clustered = FALSE, labelFunction = labelFunction1,
             balloonFunction = balloonFunction),
    addGraph(fillAlphas = .8, lineAlpha = .2, type = 'column', valueField = 'female',
             title = 'Female', clustered = FALSE, labelFunction = labelFunction1,
             balloonFunction = balloonFunction),
    setCategoryAxis(gridPosition = 'start', gridAlpha = .2, axisAlpha = 0),
    setBalloon(fixedPosition = TRUE),
    setChartCursor(valueBalloonEnabled = FALSE, cursorAlpha = .05, fullWidth = TRUE),
    addLabel(text = 'Male', x = '28%', y = '97%', bold = TRUE, align = 'middle'),
    addLabel(text = 'Female', x = '75%', y = '97%', bold = TRUE, align = 'middle'),
    addGuide(value = 0, lineAlpha = .2, valueAxis = 'axis1'),
    setDataProvider(dataProvider = dataProvider)
  )
})



output$code_bar6 <- renderText({
"
  ##Data
  n <- 15
  (male <- round(sort(runif(n)), 1))
  (female <- -round(sort(runif(n)), 1))
  (category <- factor(paste('cat.', 1:15)))
  dataProvider <- data.table(male, female, category)

  ##Javascript
  labelFunction1 <- htmlwidgets::JS('function(item) {',
  'return Math.abs(item.values.value);',
  '}')
  balloonFunction <- htmlwidgets::JS('function(item) {',
  'return item.category + \': \' + Math.abs(item.values.value) + \'%\';',
  '}')
  labelFunction2 <- htmlwidgets::JS('function(value) {',
  'return Math.abs(value) + \'%\';',
  '}')
  
  valueAxis_obj <- valueAxis(gridAlpha = 0, ignoreAxisWidth = TRUE, id = 'axis1',
  labelFunction = labelFunction2)
  
  ##Plot
  pipeline(
    amSerialChart(startDuration = 0, rotate = TRUE, marginBottom = 50,
    categoryField = 'category'),
    addGraph(fillAlphas = .8, lineAlpha = .2, type = 'column', valueField = 'male',
    title = 'Male', clustered = FALSE, labelFunction = labelFunction1,
    balloonFunction = balloonFunction),
    addGraph(fillAlphas = .8, lineAlpha = .2, type = 'column', valueField = 'female',
    title = 'Female', clustered = FALSE, labelFunction = labelFunction1,
    balloonFunction = balloonFunction),
    setCategoryAxis(gridPosition = 'start', gridAlpha = .2, axisAlpha = 0),
    setBalloon(fixedPosition = TRUE),
    setChartCursor(valueBalloonEnabled = FALSE, cursorAlpha = .05, fullWidth = TRUE),
    addLabel(text = 'Male', x = '28%', y = '97%', bold = TRUE, align = 'middle'),
    addLabel(text = 'Female', x = '75%', y = '97%', bold = TRUE, align = 'middle'),
    addGuide(value = 0, lineAlpha = .2, valueAxis = 'axis1'),
    setDataProvider(dataProvider = dataProvider)
  )
  "
})