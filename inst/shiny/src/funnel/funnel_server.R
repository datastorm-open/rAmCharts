output$funnel1 <- rAmCharts::renderAmCharts({
  pipeR::pipeline(
    amFunnelChart(neckHeight = '30%', neckWidth = '40%', titleField = 'title',
                  valueField = 'value', creditsPosition = 'bottom-left'),
    setDataProvider(data.frame(title = c('Website visits', 'Downloads'),
                               value = c(300, 123))),
    plot()
  )
})

output$code_funnel1 <- renderText({
  "
  pipeR::pipeline(
    amFunnelChart(neckHeight = '30%', neckWidth = '40%', titleField = 'title',
                  valueField = 'value', creditsPosition = 'bottom-left'),
    setDataProvider(data.frame(title = c('Website visits', 'Downloads'),
                               value = c(300, 123))),
    plot()
  )
  "
})