output$funnel1 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amFunnelChart(theme = 'light', neckHeight = '30%', neckWidth = '40%', titleField = 'title',
                  valueField = 'value', creditsPosition = 'bottom-left'),
    setDataProvider(data.table(title = c('Website visits', 'nb. vistors',  'Downloads'),
                               value = c(300, 200, 123))),
    plot()
  )
})

output$code_funnel1 <- renderText({
  "
  pipeR::pipeline(
    amFunnelChart(neckHeight = '30%', neckWidth = '40%', titleField = 'title',
                  valueField = 'value', creditsPosition = 'bottom-left'),
    setDataProvider(data.table(title = c('Website visits', 'nb. vistors',  'Downloads'),
                               value = c(300, 200, 123))),
    plot()
  )
  "
})