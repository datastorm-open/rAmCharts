output$amfunnel1 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_funnel')
  ##Plot
  amFunnel(data = data_funnel, inverse = TRUE)
  
})

output$code_amfunnel1 <- renderText({
  "
  ##Data
  data('data_funnel')
  ##Plot
  amFunnel(data = data_funnel, inverse = TRUE)
  "
})



output$amfunnel2 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_funnel')
  ##Plot
  #Change the orientation and legend side              
  amFunnel(data = data_funnel, main = "Reversed Pyramid", inverse = FALSE,
           label_side = "left", margin_right = 15, margin_left = 160)
  
  
})

output$code_amfunnel2 <- renderText({
  "
  ##Data
  data('data_funnel')
  ##Plot
  amFunnel(data = data_funnel, main = 'Reversed Pyramid', inverse = FALSE,
     label_side = 'left', margin_right = 15, margin_left = 160)
  "
})




output$amfunnel3 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_funnel')
  ##Plot
  #Change the orientation and legend side              
  amFunnel(data = data_funnel, neck_height = 30, neck_width = 40)
  
  
})

output$code_amfunnel3 <- renderText({
  "
  ##Data
  data('data_funnel')
  ##Plot
  amFunnel(data = data_funnel, neck_height = 30, neck_width = 40)
  "
})


output$amfunnel4 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_funnel')
  ##Plot
  #Change the orientation and legend side              
  amFunnel(data = data_funnel, third_dim = TRUE, inverse = TRUE)
  
  
})

output$code_amfunnel4 <- renderText({
  "
  ##Data
  data('data_funnel')
  ##Plot
  amFunnel(data = data_funnel, third_dim = TRUE, inverse = TRUE)
  "
})



