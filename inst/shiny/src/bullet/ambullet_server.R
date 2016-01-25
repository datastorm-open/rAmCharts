output$amBullet0 <- rAmCharts::renderAmCharts({
  amBullet(value = 65)
})

output$code_amBullet0 <- renderText({
  "
  ##Plot
  amBullet(value = 65)
  "
})

output$amBullet0 <- rAmCharts::renderAmCharts({
  amBullet(value = 65)
})

output$code_amBullet0 <- renderText({
  "
  ##Plot
  amBullet(value = 65)
  "
})

output$amBullet1 <- rAmCharts::renderAmCharts({
  ##Plot
  amBullet(value = 65, val_color = "purple", limit_color = "#3c8dbc", step = FALSE, limit = 66)
})

output$code_amBullet1 <- renderText({
  "
  ##Plot
  amBullet(value = 65, val_color = 'purple', limit_color = '#3c8dbc', step = FALSE, limit = 66)
  "
})

output$amBullet2 <- rAmCharts::renderAmCharts({
  ## Plot
  amBullet(value = 65, main = 'Bullet chart 1', mainSize = 15)
})

output$code_amBullet2 <- renderText({
  "
  ## Plot
  amBullet(value = 65, main = 'Bullet chart 1', mainSize = 15)
  "
})