

output$amRadar0 <- rAmCharts::renderAmCharts({
  ##Data
  dp <- data.frame(label = c('Blue', 'Brown', 'Green', 'Hazel'),
                   Male = c(101, 98, 33, 47), Female = c(114, 122, 31, 46))
  ##Plot
  amRadar(dp)
})

output$code_amRadar0 <- renderText({
  "
  ##Data
  dp <- data.frame(label = c('Blue', 'Brown', 'Green', 'Hazel'),
                   Male = c(101, 98, 33, 47), Female = c(114, 122, 31, 46))
  ##Plot
  amRadar(dp)
  "
})



output$amRadar1 <- rAmCharts::renderAmCharts({
  ##Data
  data('data_radar')
  ##Plot
  amRadar(data_radar, main = 'My title',
          col = c('#0000FF', '#00FF00', '#FF0000'), backTransparency = c(0, 0.4, 0.2),
          type = 'polygons', pch = 'triangleRight')
})

output$code_amRadar1 <- renderText({
  "
  ##Data
  data('data_radar')
  ##Plot
  amRadar(data_radar, main = 'My title',
          col = c('#0000FF', '#00FF00', '#FF0000'), backTransparency = c(0, 0.4, 0.2),
          type = 'polygons', pch = 'triangleRight')
  "
})

output$amWind <- rAmCharts::renderAmCharts({
  ##Data
  data('data_wind')
  ##Plot
  amWind(data = data_wind, col = c('#0404B4', '#01DF01', '#FFBF00'),
         backTransparency = 1, pch = 'round')
})

output$code_amWind <- renderText({
  "
  ##Data
  data('data_wind')
  ##Plot
  amWind(data = data_wind, col = c('#0404B4', '#01DF01', '#FFBF00'),
         backTransparency = 1, pch = 'round')
  "
})
