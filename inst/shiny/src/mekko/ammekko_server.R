output$ammekko0 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_mekko')
##Plot
amMekko(x = 'var1', y = 'var2', data = data_mekko)
})


output$code_ammekko0 <- renderText({
  "
  ##Data
  data('data_mekko')
  ##Plot
  amMekko(x = 'var1', y = 'var2', data = data_mekko)
  "
})
  
  
  
output$ammekko1 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_mekko')
  ##Plot
  amMekko(x = "var1", y = "var2", data = data_mekko, horiz = TRUE, show_values = TRUE)
})


output$code_ammekko1 <- renderText({
  "
  ##Data
  data('data_mekko')
  ##Plot
amMekko(x = 'var1', y = 'var2', data = data_mekko, horiz = TRUE, show_values = TRUE)
  "
})


output$ammekko2 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_mekko')
  ##Plot
  amMekko(x = "var1", y = "var2", data = data_mekko, legend = TRUE)
})


output$code_ammekko2 <- renderText({
  "
  ##Data
  data('data_mekko')
  ##Plot
  amMekko(x = 'var1', y = 'var2', data = data_mekko, legend = TRUE)
  "
})
  
