
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
  amBarplot(x = 'year', y = c('income', 'expenses'), data = data_gbar, stack_type = '100', legend = TRUE, legend_side = 'left')
})



output$code_amBar3 <- renderText({
  "
 ##Data
  data('data_gbar')
  
  ##Plot
  amBarplot(x = 'year', y = c('income', 'expenses'), data = data_gbar, stack_type = '100', legend = TRUE, legend_side = 'left')
  "
})





# 
# amBarplot(x = "country", y = "visits", data = data_bar, show_values = TRUE)



# 
# amBarplot(x = "country", y = "visits", data = data_bar)
# 
# #horizontal bar
# amBarplot(x = "country", y = "visits", data = data_bar, horiz = TRUE)
# 
# #3D bar
# amBarplot(x = "country", y = "visits", data = data_bar, third_dim = TRUE)
# 
# #display values
# amBarplot(x = "country", y = "visits", data = data_bar, show_values = TRUE)
# 
# #grouped columns
# data_gbar <- data.frame(year = c("2005", "2006", "2007", "2008", "2009"),
#                         income = c(23.5, 26.2, 30.1, 29.5, 24.6),
#                         expenses = c(18.1, 22.8, 23.9, 25.1, 25),
#                         stringsAsFactors = FALSE)
# 
# amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar)
# 
# #add legend
# amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, legend = TRUE)
# 
# #change groups colors
# amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, 
#       groups_color = c("#87cefa", "#c7158"), legend = TRUE)
# 
# #stacked bars
# amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, stack_type = "regular")
# 
# #100% stacked bars
# amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, stack_type = "100")
# 
# #layered bars
# amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, layered = TRUE, show_values = TRUE)
# 


