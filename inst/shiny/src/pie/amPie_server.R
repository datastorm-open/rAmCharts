

output$ampie0 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_gdp')
  setnames(data_gdp, c("country", "gdp"), c("label", "value"))
  ##Plot
  amPie(data_gdp)
})

output$code_ampie0 <- renderText({
  "
  ##Data
  data('data_gdp')
  setnames(data_gdp, c('country', 'gdp'), c('label', 'value'))
  ##Plot
  amPie(data_gdp)
  "
})






output$ampie1 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_gdp')
  setnames(data_gdp, c('country','gdp'), c('label', 'value'))
  color <- rgb(sample(255)[1:10], sample(255)[1:10], sample(255)[1:10], maxColorValue=255)
  data_gdp$color <- color
  ##Plot
  amPie(data_gdp)
})

output$code_ampie1 <- renderText({
  "
  ##Data
  data('data_gdp')
  setnames(data_gdp,c('country', 'gdp'), c('label','value'))
  color <- rgb(sample(255)[1:10], sample(255)[1:10], sample(255)[1:10], maxColorValue=255)
  data_gdp$color <- color
  ##Plot
  amPie(data_gdp)
  "
})



output$ampie2 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_gdp')
  setnames(data_gdp,c('country', 'gdp'), c('label', 'value'))
  ##Plot
  amPie(data_gdp, legend = TRUE)
})

output$code_ampie2 <- renderText({
  "
  ##Data
  data('data_gdp')
  setnames(data_gdp,c('country', 'gdp'), c('label', 'value'))
  ##Plot
  amPie(data_gdp, legend = TRUE)
  "
})





output$ampie3 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_gdp')
  setnames(data_gdp,c('country', 'gdp'), c('label', 'value'))
  ##Plot
  amPie(data_gdp, legend = TRUE,inner_radius = 45,third_dim = TRUE,show_values = FALSE)
})

output$code_ampie3 <- renderText({
  "
  ##Data
  data('data_gdp')
  setnames(data_gdp,c('country', 'gdp'), c('label', 'value'))
  ##Plot
  amPie(data_gdp, legend = TRUE,inner_radius = 45,third_dim = TRUE,show_values = FALSE)
  "
})


