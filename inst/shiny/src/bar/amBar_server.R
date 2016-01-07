
output$amBar0 <- rAmCharts::renderAmCharts({
  ##Data
  data_bar <- data.frame(
    country = c('USA', 'China', 'Japan', 'Germany','UK', 'France', 'India',
                'Spain','Netherlands', 'Russia', 'South Korea','Canada'),
    visits = c(3025, 1882, 1809, 1322, 1122, 1114, 984, 711, 665, 580, 443, 441),
    stringsAsFactors = FALSE
    )
  ##Plot
  amBar(x = 'country', y = 'visits', data = data_bar)
})



output$code_amBar0 <- renderText({
  "
  ##Data
  data_bar <- data.frame(
    country = c('USA', 'China', 'Japan', 'Germany','UK', 'France', 'India', 'Spain','Netherlands', 'Russia', 'South Korea','Canada'),
    visits = c(3025, 1882, 1809, 1322, 1122, 1114, 984, 711, 665, 580, 443, 441),
    stringsAsFactors = FALSE
  )
  ##Plot
  amBar(x = 'country', y = 'visits', data = data_bar)
  "
})






output$amBar1 <- rAmCharts::renderAmCharts({
  ##Data
  data_bar <- data.frame(
    country = c('USA', 'China', 'Japan', 'Germany','UK', 'France',
                'India', 'Spain','Netherlands', 'Russia', 'South Korea','Canada'),
    visits = c(3025, 1882, 1809, 1322, 1122, 1114, 984, 711, 665, 580, 443, 441),
    color = c('#FF0F00', '#FF6600', '#FF9E01', '#FCD202', '#F8FF01', '#B0DE09', 
              '#04D215', '#0D8ECF', '#0D52D1', '#2A0CD0', '#8A0CCF', '#CD0D74'),
    stringsAsFactors = FALSE
  )
  ##Plot
  amBar(x = 'country', y = 'visits', data = data_bar, horiz = TRUE)
})



output$code_amBar1 <- renderText({
  "
  ##Data
  data_bar <- data.frame(
    country = c('USA', 'China', 'Japan', 'Germany','UK', 'France', 
    'India', 'Spain','Netherlands', 'Russia', 'South Korea','Canada'),
    visits = c(3025, 1882, 1809, 1322, 1122, 1114, 984, 711, 665, 580, 443, 441),
    color = c('#FF0F00', '#FF6600', '#FF9E01', '#FCD202', '#F8FF01', 
    '#B0DE09', '#04D215', '#0D8ECF', '#0D52D1', '#2A0CD0', '#8A0CCF', '#CD0D74'),
    stringsAsFactors = FALSE
  )
  ##Plot
  amBar(x = 'country', y = 'visits', data = data_bar, horiz = TRUE)
  "
})

output$amBar2 <- rAmCharts::renderAmCharts({
  ##Data
  data_gbar <- data.frame(year = c('2005', '2006', '2007', '2008', '2009'),
                        income = c(23.5, 26.2, 30.1, 29.5, 24.6),
                        expenses = c(18.1, 22.8, 23.9, 25.1, 25),
                        stringsAsFactors = FALSE)

  ##Plot
  amBar(x = 'year', y = c('income', 'expenses'), data = data_gbar, layered = TRUE, show_values = TRUE)
})





output$code_amBar2 <- renderText({
  "
  ##Data
  data_gbar <- data.frame(year = c('2005', '2006', '2007', '2008', '2009'),
    income = c(23.5, 26.2, 30.1, 29.5, 24.6),
    expenses = c(18.1, 22.8, 23.9, 25.1, 25),
    stringsAsFactors = FALSE
  )
  
  ##Plot
  amBar(x = 'year', y = c('income', 'expenses'), data = data_gbar, layered = TRUE, show_values = TRUE)
  "
})


output$amBar3 <- rAmCharts::renderAmCharts({
  ##Data
  data_gbar <- data.frame(year = c('2005', '2006', '2007', '2008', '2009'),
                          income = c(23.5, 26.2, 30.1, 29.5, 24.6),
                          expenses = c(18.1, 22.8, 23.9, 25.1, 25),
                          stringsAsFactors = FALSE)
  
  ##Plot
  amBar(x = 'year', y = c('income', 'expenses'), data = data_gbar, stack_type = '100', legend = TRUE, legend_side = 'left')
})



output$code_amBar3 <- renderText({
  "
 ##Data
  data_gbar <- data.frame(year = c('2005', '2006', '2007', '2008', '2009'),
    income = c(23.5, 26.2, 30.1, 29.5, 24.6),
    expenses = c(18.1, 22.8, 23.9, 25.1, 25),
    stringsAsFactors = FALSE
  )
  
  ##Plot
  amBar(x = 'year', y = c('income', 'expenses'), data = data_gbar, stack_type = '100', legend = TRUE, legend_side = 'left')
  "
})





# 
# amBar(x = "country", y = "visits", data = data_bar, show_values = TRUE)



# 
# amBar(x = "country", y = "visits", data = data_bar)
# 
# #horizontal bar
# amBar(x = "country", y = "visits", data = data_bar, horiz = TRUE)
# 
# #3D bar
# amBar(x = "country", y = "visits", data = data_bar, third_dim = TRUE)
# 
# #display values
# amBar(x = "country", y = "visits", data = data_bar, show_values = TRUE)
# 
# #grouped columns
# data_gbar <- data.frame(year = c("2005", "2006", "2007", "2008", "2009"),
#                         income = c(23.5, 26.2, 30.1, 29.5, 24.6),
#                         expenses = c(18.1, 22.8, 23.9, 25.1, 25),
#                         stringsAsFactors = FALSE)
# 
# amBar(x = "year", y = c("income", "expenses"), data = data_gbar)
# 
# #add legend
# amBar(x = "year", y = c("income", "expenses"), data = data_gbar, legend = TRUE)
# 
# #change groups colors
# amBar(x = "year", y = c("income", "expenses"), data = data_gbar, 
#       groups_color = c("#87cefa", "#c7158"), legend = TRUE)
# 
# #stacked bars
# amBar(x = "year", y = c("income", "expenses"), data = data_gbar, stack_type = "regular")
# 
# #100% stacked bars
# amBar(x = "year", y = c("income", "expenses"), data = data_gbar, stack_type = "100")
# 
# #layered bars
# amBar(x = "year", y = c("income", "expenses"), data = data_gbar, layered = TRUE, show_values = TRUE)
# 


