output$funnel1 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_funnel')
  ##Plot
  amFunnelChart(dataProvider = data_funnel, startDuration = 0,
                labelPosition = 'left', marginLeft = 200, marginRight = 100,
                titleField = 'description', valueField = 'value')
})

output$code_funnel1 <- renderText({
  "
  ##Data
  data('data_funnel')
  ##Plot
  amFunnelChart(dataProvider = data_funnel, startDuration = 0,
                labelPosition = 'left', marginLeft = 200, marginRight = 100,
                titleField = 'description', valueField = 'value')
  "
})

output$funnel2 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_funnel')
  ##Plot
  amFunnelChart(dataProvider = data_funnel, startX = -500,
                depth3D = 50, angle = 60, outlineColor = '#FFFFFF',
                outLineThickness = 2, titleField = 'description', valueField = 'value',
                labelPosition = 'right', marginLeft = 100,
                marginRight = 300, startDuration = 0)
})

output$code_funnel2 <- renderText({
  "
  ##Data
  data('data_funnel')
  ##Plot
  amFunnelChart(dataProvider = data_funnel, startX = -500,
                depth3D = 50, angle = 60, outlineColor = '#FFFFFF',
                outLineThickness = 2, titleField = 'description', valueField = 'value',
                labelPosition = 'right', marginLeft = 100,
                marginRight = 300, startDuration = 0)
  "
})

output$funnel3 <- rAmCharts::renderAmCharts({
  ## Data
  data('data_funnel')
  
  ## Plot
  amFunnelChart(dataProvider = data_funnel, startX = -500,
                titleField = 'description', valueField = 'value',
                labelPosition = 'right', marginLeft = 100,
                marginRight = 300, rotate = TRUE, startDuration = 0)
})

output$code_funnel3 <- renderText({
  "
  ## Data
  data('data_funnel')
  
  ## Plot
  amFunnelChart(dataProvider = data_funnel, startX = -500,
                titleField = 'description', valueField = 'value',
                labelPosition = 'right', marginLeft = 100,
                marginRight = 300, rotate = TRUE, startDuration = 0)
  "
})