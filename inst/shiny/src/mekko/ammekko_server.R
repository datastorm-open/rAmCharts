output$ammekko0 <- rAmCharts::renderAmCharts({
  ##Data
data_mekko <- data.frame(var1 = c(rep('A1', 150), rep('A2', 350), rep('A3', 500)),
                         var2 = sample(c('B1', 'B2', 'B3', 'B4', 'B5', 'B6'), 
                                       1000, replace = TRUE))
##Graph
amMekko(x = 'var1', y = 'var2', data = data_mekko)
})


output$code_ammekko0 <- renderText({
  "
  ##Data
  data_mekko <- data.frame(var1 = c(rep('A1', 150), rep('A2', 350), rep('A3', 500)),
    var2 = sample(c('B1', 'B2', 'B3', 'B4', 'B5', 'B6'), 
  1000, replace = TRUE))
  ##Graph
  amMekko(x = 'var1', y = 'var2', data = data_mekko)
  "
})
  
  
  
output$ammekko1 <- rAmCharts::renderAmCharts({
  ##Data
  data_mekko <- data.frame(var1 = c(rep('A1', 150), rep('A2', 350), rep('A3', 500)),
                           var2 = sample(c('B1', 'B2', 'B3', 'B4', 'B5', 'B6'), 
                                         1000, replace = TRUE))
  ##Graph
  amMekko(x = "var1", y = "var2", data = data_mekko, horiz = TRUE, show_values = TRUE)
})


output$code_ammekko1 <- renderText({
  "
  ##Data
  data_mekko <- data.frame(var1 = c(rep('A1', 150), rep('A2', 350), rep('A3', 500)),
  var2 = sample(c('B1', 'B2', 'B3', 'B4', 'B5', 'B6'), 
  1000, replace = TRUE))
  ##Graph
amMekko(x = 'var1', y = 'var2', data = data_mekko, horiz = TRUE, show_values = TRUE)
  "
})


output$ammekko2 <- rAmCharts::renderAmCharts({
  ##Data
  data_mekko <- data.frame(var1 = c(rep('A1', 150), rep('A2', 350), rep('A3', 500)),
                           var2 = sample(c('B1', 'B2', 'B3', 'B4', 'B5', 'B6'), 
                                         1000, replace = TRUE))
  amMekko(x = "var1", y = "var2", data = data_mekko, legend = TRUE)
})


output$code_ammekko2 <- renderText({
  "
  ##Data
  data_mekko <- data.frame(var1 = c(rep('A1', 150), rep('A2', 350), rep('A3', 500)),
    var2 = sample(c('B1', 'B2', 'B3', 'B4', 'B5', 'B6'), 
  1000, replace = TRUE))
  ##Graph
  amMekko(x = 'var1', y = 'var2', data = data_mekko, legend = TRUE)
  "
})
  
