
output$amBar0 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_bar')
  ##Plot
  amBarplot(x = 'country', y = 'visits', data = data_bar)
})



output$code_amBar0 <- renderText({
  "
  ##Data
  data('data_bar')
  ##Plot
  amBarplot(x = 'country', y = 'visits', data = data_bar)
  "
})






output$amBar1 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_bar')
  ##Plot
  amBarplot(x = 'country', y = 'visits', data = data_bar, horiz = TRUE)
})



output$code_amBar1 <- renderText({
  "
  ##Data
  data('data_bar')
  ##Plot
  amBarplot(x = 'country', y = 'visits', data = data_bar, horiz = TRUE)
  "
})

output$amBar2 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_gbar')
  ##Plot
  amBarplot(x = 'year', y = c('income', 'expenses'), data = data_gbar, layered = TRUE, show_values = TRUE)
})





output$code_amBar2 <- renderText({
  "
  ##Data
  data('data_gbar')
  ##Plot
  amBarplot(x = 'year', y = c('income', 'expenses'), data = data_gbar, layered = TRUE, show_values = TRUE)
  "
})


output$amBar3 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_gbar')
  ##Plot
  amBarplot(x = 'year', y = c('income', 'expenses'), data = data_gbar, stack_type = '100', legend = TRUE, legendPosition = 'left')
})



output$code_amBar3 <- renderText({
  "
 ##Data
  data('data_gbar')
  ##Plot
  amBarplot(x = 'year', y = c('income', 'expenses'), data = data_gbar, stack_type = '100', legend = TRUE, legendPosition = 'left')
  "
})

