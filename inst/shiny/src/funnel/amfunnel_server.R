output$amfunnel1 <- rAmCharts::renderAmCharts({
  ## Data
  data('data_funnel')
  
  ## Plot
  amFunnel(data = data_funnel, inverse = TRUE, margin_right = 250)
})

output$code_amfunnel1 <- renderText({
  "
  ## Data
  data('data_funnel')
  
  ## Plot
  amFunnel(data = data_funnel, inverse = TRUE, margin_right = 250)
  "
})

output$amfunnel2 <- rAmCharts::renderAmCharts({
  ## Data
  data('data_funnel')
  
  ## Plot
  amFunnel(data = data_funnel, main = 'Reversed Pyramid', inverse = FALSE,
           label_side = 'left', margin_right = 200)
})

output$code_amfunnel2 <- renderText({
  "
  ## Data
  data('data_funnel')
  
  ## Plot
  amFunnel(data = data_funnel, main = 'Reversed Pyramid', inverse = FALSE,
           label_side = 'left', margin_right = 200)
  "
})

output$amfunnel3 <- rAmCharts::renderAmCharts({
  ## Data
  data('data_funnel')
  
  ## Plot
  amFunnel(data = data_funnel, neck_height = 30, neck_width = 40, margin_right = 250)
})

output$code_amfunnel3 <- renderText({
  "
  ## Data
  data('data_funnel')
  
  ## Plot
  amFunnel(data = data_funnel, neck_height = 30, neck_width = 40, margin_right = 250)
  "
})

# output$amfunnel4 <- rAmCharts::renderAmCharts({
#   ## Data
#   data('data_funnel')
#   
#   ## Plot
#   amFunnel(data = data_funnel, third_dim = TRUE, inverse = TRUE, margin_right = 250)
# })
# 
# output$code_amfunnel4 <- renderText({
#   "
#   ##Data
#   data('data_funnel')
#   ##Plot
#   amFunnel(data = data_funnel, third_dim = TRUE, inverse = TRUE)
#   "
# })



