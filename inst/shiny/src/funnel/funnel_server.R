output$funnel1 <- rAmCharts::renderAmCharts({
  # prepare data
  dp <- data.table(title = c('Website visits', 'Downloads',
                             'Requested prices', 'contacted',
                             'Prurchased', 'Asked for support',
                             'Purchased more'),
                   value = c(300, 123, 98, 72, 35, 25, 18))
  
  # build chart
  pipeR::pipeline(
    amFunnelChart(theme = input$theme_funnel, dataProvider = dp,
                  neckHeight = '20%', neckWidth = '40%',
                  titleField = 'title', valueField = 'value', labelPosition = 'right',
                  creditsPosition = 'bottom-left', marginLeft = 15, marginRight = 200,
                  funnelAlpha = .9, startAlpha = 0, startDuration = 0)
  )
})

output$code_funnel1 <- renderText({
  "
  # prepare data
  dp <- data.table(title = c('Website visits', 'Downloads',
                             'Requested prices', 'contacted',
                             'Prurchased', 'Asked for support',
                             'Purchased more'),
                   value = c(300, 123, 98, 72, 35, 25, 18))
  
  # build chart
  pipeR::pipeline(
    amFunnelChart(theme = input$theme_funnel, dataProvider = dp,
                  neckHeight = '20%', neckWidth = '40%',
                  titleField = 'title', valueField = 'value', labelPosition = 'right',
                  creditsPosition = 'bottom-left', marginLeft = 15, marginRight = 200,
                  funnelAlpha = .9, startAlpha = 0, startDuration = 0)
  )
  "
})

output$funnel2 <- rAmCharts::renderAmCharts({
  # prepare data
  dp <- data.table(title = c('Website visits', 'Downloads',
                             'Requested prices', 'contacted',
                             'Prurchased', 'Asked for support',
                             'Purchased more'),
                   value = c(300, 123, 98, 72, 35, 25, 18))
  
  # build chart
  pipeR::pipeline(
    amFunnelChart(dataProvider = dp, startX = -500, depth3D = input$depth_funnel,
                  angle = input$angle_funnel, outlineColor = '#FFFFFF', outLineThickness = 2,
                  titleField = 'title', valueField = 'value', labelPosition = 'right',
                  marginLeft = 100, marginRight = 300,
                  startDuration = 0)
  )
})

output$code_funnel2 <- renderText({
  "
  # prepare data
  dp <- data.table(title = c('Website visits', 'Downloads',
                             'Requested prices', 'contacted',
                             'Prurchased', 'Asked for support',
                             'Purchased more'),
                   value = c(300, 123, 98, 72, 35, 25, 18))
  
  # build chart
  pipeR::pipeline(
    amFunnelChart(dataProvider = dp, startX = -500, depth3D = input$depth_funnel,
                  angle = input$angle_funnel, outlineColor = '#FFFFFF', outLineThickness = 2,
                  titleField = 'title', valueField = 'value', labelPosition = 'right',
                  marginLeft = 100, marginRight = 300,
                  startDuration = 0)
  )
  "
})

output$funnel3 <- rAmCharts::renderAmCharts({
  # prepare data
  dp <- data.table(title = c('Website visits', 'Downloads',
                             'Requested prices', 'contacted',
                             'Prurchased', 'Asked for support',
                             'Purchased more'),
                   value = c(300, 123, 98, 72, 35, 25, 18))
  
  # build chart
  pipeR::pipeline(
    amFunnelChart(dataProvider = dp, startX = -500, titleField = 'title',
                  valueField = 'value', labelPosition = 'right',
                  marginLeft = 100, marginRight = 300, rotate = TRUE,
                  startDuration = 0)
  )
})

output$code_funnel3 <- renderText({
  "
  # prepare data
  dp <- data.table(title = c('Website visits', 'Downloads',
                             'Requested prices', 'contacted',
                             'Prurchased', 'Asked for support',
                             'Purchased more'),
                   value = c(300, 123, 98, 72, 35, 25, 18))
  
  # build chart
  pipeR::pipeline(
    amFunnelChart(dataProvider = dp, startX = -500, titleField = 'title',
                  valueField = 'value', labelPosition = 'right',
                  marginLeft = 100, marginRight = 300, rotate = TRUE,
                  startDuration = 0)
  )
  "
})