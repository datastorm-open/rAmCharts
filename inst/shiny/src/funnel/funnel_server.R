output$funnel1 <- rAmCharts::renderAmCharts({
  # prepare data
  data('data_funnel')
  
  # build chart
  pipeR::pipeline(
    amFunnelChart( dataProvider = data_funnel,
                  titleField = 'title', valueField = 'value')
  )
})

output$code_funnel1 <- renderText({
  "
  ##Data
  data('data_funnel')
  
  ##Plot
  pipeR::pipeline(
  amFunnelChart( dataProvider = data_funnel,
    titleField = 'title', valueField = 'value')
  )
  "
})

output$funnel2 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_funnel')
  
  ##Plot
  pipeR::pipeline(
    amFunnelChart(dataProvider = data_funnel, startX = -500, depth3D = 50,
                  angle = 60, outlineColor = '#FFFFFF', outLineThickness = 2,
                  titleField = 'title', valueField = 'value', labelPosition = 'right',
                  marginLeft = 100, marginRight = 300,
                  startDuration = 0)
  )
})

output$code_funnel2 <- renderText({
  "
  ##Data
  data('data_funnel')
  
  ##Plot
  pipeR::pipeline(
    amFunnelChart(dataProvider = data_funnel, startX = -500, depth3D = 50,
    angle = 60, outlineColor = '#FFFFFF', outLineThickness = 2,
    titleField = 'title', valueField = 'value', labelPosition = 'right',
    marginLeft = 100, marginRight = 300,
    startDuration = 0)
  )
  "
})

output$funnel3 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_funnel')
  
  ##Plot
  pipeR::pipeline(
    amFunnelChart(dataProvider = data_funnel, startX = -500, titleField = 'title',
                  valueField = 'value', labelPosition = 'right',
                  marginLeft = 100, marginRight = 300, rotate = TRUE,
                  startDuration = 0)
  )
})

output$code_funnel3 <- renderText({
  "
  ##Data
  data('data_funnel')
  
  ##Plot
  pipeR::pipeline(
    amFunnelChart(dataProvider = data_funnel, startX = -500, titleField = 'title',
    valueField = 'value', labelPosition = 'right',
    marginLeft = 100, marginRight = 300, rotate = TRUE,
    startDuration = 0)
  )
  "
})