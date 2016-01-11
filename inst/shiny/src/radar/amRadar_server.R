

output$amRadar0 <- rAmCharts::renderAmCharts({
  # prepare data
  dp <- data.frame(label = c('Blue', 'Brown', 'Green', 'Hazel')
                   , Male = c(101, 98, 33, 47), Female = c(114, 122, 31, 46))
  #Graph
  amRadar(dp)
})

output$code_amRadar0 <- renderText({
  "
  # prepare data
  dp <- data.frame(label = c('Blue', 'Brown', 'Green', 'Hazel'), Male = c(101, 98, 33, 47), Female = c(114, 122, 31, 46))
  #Graph
  amRadar(dp)
  "
})



output$amRadar1 <- rAmCharts::renderAmCharts({
  # prepare data
  data <- data.frame(label = c('A', 'Z', 'E', 'R', 'T'),
                     Product1 = c(1, 2, 3, 4, 2), Product2 = c(2, 8, 1, 1, 0),Product3 = c(1,1,2,2,4))
  #Graph
  amRadar(data, main = 'My title', export = TRUE,
          col = c('#0000FF','#00FF00','#FF0000'), backTransparency = c(0,0.4),
          type = c('polygons'),pch='triangleRight')
})

output$code_amRadar1 <- renderText({
  "
  # prepare data
  data <- data.frame(label = c('A', 'Z', 'E', 'R', 'T'), Product1 = c(1, 2, 3, 4, 2), Product2 = c(2, 8, 1, 1, 0),Product3 = c(1,1,2,2,4))
  #Graph
  amRadar(data, main = 'My title', export = TRUE, col = c('#0000FF','#00FF00','#FF0000'), backTransparency = c(0,0.4),
  type = c('polygons'),pch='triangleRight')
  "
})

output$amWind <- rAmCharts::renderAmCharts({
  # prepare data
  data <- data.frame(Week = c(1, 2, 3, 4, 1, 2, 1, 2), 
                     Middle = c(2, 8, 1, 1, 2, 8, 1, 2),Strong = c(1, 1, 2, 2, 1, 1 ,1, 2))
  #Graph
  amWind(data, main = '', export = TRUE,
         col = c('#0404B4', '#01DF01', '#FFBF00'), backTransparency = 1 ,pch='round')
})

  output$code_amWind <- renderText({
    "
  # prepare data
  data <- data.frame(Week = c(1, 2, 3, 4, 1, 2, 1, 2), Middle = c(2, 8, 1, 1, 2, 8, 1, 2),Strong = c(1, 1, 2, 2, 1, 1 ,1, 2))
    #Graph
    amWind(data, main = '', export = TRUE, col = c('#0404B4', '#01DF01', '#FFBF00'), backTransparency = 1 ,pch='round')
    "
  })
  