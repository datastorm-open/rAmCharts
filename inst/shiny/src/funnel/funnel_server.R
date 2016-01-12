output$funnel1 <- rAmCharts::renderAmCharts({
  # prepare data
  dp <- data.table(title = c('Website visits', 'Downloads',
                             'Requested prices', 'contacted',
                             'Prurchased', 'Asked for support',
                             'Purchased more'),
                   value = c(300, 123, 98, 72, 35, 25, 18))
  
  # build chart
  pipeR::pipeline(
    amFunnelChart( dataProvider = dp,
                  titleField = 'title', valueField = 'value')
  )
})

output$code_funnel1 <- renderText({
  "
  ##Data
  dp <- data.table(title = c('Website visits', 'Downloads',
    'Requested prices', 'contacted',
    'Prurchased', 'Asked for support',
    'Purchased more'),
    value = c(300, 123, 98, 72, 35, 25, 18)
  )
  
  ##Plot
  pipeR::pipeline(
  amFunnelChart( dataProvider = dp,
    titleField = 'title', valueField = 'value')
  )
  "
})

output$funnel2 <- rAmCharts::renderAmCharts({
  ##Data
  dp <- data.table(title = c('Website visits', 'Downloads',
                             'Requested prices', 'contacted',
                             'Prurchased', 'Asked for support',
                             'Purchased more'),
                   value = c(300, 123, 98, 72, 35, 25, 18))
  
  ##Plot
  pipeR::pipeline(
    amFunnelChart(dataProvider = dp, startX = -500, depth3D = 50,
                  angle = 60, outlineColor = '#FFFFFF', outLineThickness = 2,
                  titleField = 'title', valueField = 'value', labelPosition = 'right',
                  marginLeft = 100, marginRight = 300,
                  startDuration = 0)
  )
})

output$code_funnel2 <- renderText({
  "
  ##Data
  dp <- data.table(title = c('Website visits', 'Downloads',
    'Requested prices', 'contacted',
    'Prurchased', 'Asked for support',
    'Purchased more'),
    value = c(300, 123, 98, 72, 35, 25, 18))
  
  ##Plot
  pipeR::pipeline(
    amFunnelChart(dataProvider = dp, startX = -500, depth3D = 50,
    angle = 60, outlineColor = '#FFFFFF', outLineThickness = 2,
    titleField = 'title', valueField = 'value', labelPosition = 'right',
    marginLeft = 100, marginRight = 300,
    startDuration = 0)
  )
  "
})

output$funnel3 <- rAmCharts::renderAmCharts({
  ##Data
  dp <- data.table(title = c('Website visits', 'Downloads',
                             'Requested prices', 'contacted',
                             'Prurchased', 'Asked for support',
                             'Purchased more'),
                   value = c(300, 123, 98, 72, 35, 25, 18))
  
  ##Plot
  pipeR::pipeline(
    amFunnelChart(dataProvider = dp, startX = -500, titleField = 'title',
                  valueField = 'value', labelPosition = 'right',
                  marginLeft = 100, marginRight = 300, rotate = TRUE,
                  startDuration = 0)
  )
})

output$code_funnel3 <- renderText({
  "
  ##Data
  dp <- data.table(title = c('Website visits', 'Downloads',
    'Requested prices', 'contacted',
    'Prurchased', 'Asked for support',
    'Purchased more'),
  value = c(300, 123, 98, 72, 35, 25, 18))
  
  ##Plot
  pipeR::pipeline(
    amFunnelChart(dataProvider = dp, startX = -500, titleField = 'title',
    valueField = 'value', labelPosition = 'right',
    marginLeft = 100, marginRight = 300, rotate = TRUE,
    startDuration = 0)
  )
  "
})