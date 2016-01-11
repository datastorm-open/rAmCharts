

output$ampie0 <- rAmCharts::renderAmCharts({
  amPie(data_gdp)
})

output$code_ampie0 <- renderText({
  "
  amPie(data_gdp)
  "
})






output$ampie1 <- rAmCharts::renderAmCharts({
  color <- rgb(sample(255)[1:10],sample(255)[1:10],sample(255)[1:10], maxColorValue=255)
  data_gdp$color <- color
  amPie(data_gdp)
})

output$code_ampie1 <- renderText({
  "
  color <- rgb(sample(255)[1:10],sample(255)[1:10],sample(255)[1:10], maxColorValue=255)
  data_gdp$color <- color
  amPie(data_gdp)
  "
})



output$ampie2 <- rAmCharts::renderAmCharts({
  amPie(data_gdp, legend = TRUE)
})

output$code_ampie2 <- renderText({
  "
  amPie(data_gdp, legend = TRUE)
  "
})





output$ampie3 <- rAmCharts::renderAmCharts({
  amPie(data_gdp, legend = TRUE,inner_radius = 45,third_dim = TRUE,show_values = FALSE)
})

output$code_ampie3 <- renderText({
  "
 amPie(data_gdp, legend = TRUE,inner_radius = 45,third_dim = TRUE,show_values = FALSE)
  "
})


