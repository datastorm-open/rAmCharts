#Basic example
data_pie <- data.frame(label = c("Facebook", "Twitter", "LinkedIn", "Google+", 
                                  "Pinterest"),
                       value = c(38, 25, 15, 14, 8), stringsAsFactors = FALSE)
                       
amPie(data = data_pie)


#don't display values
amPie(data = data_pie, show_values = FALSE)

#3D pie
amPie(data = data_pie, depth = 10)

#donut chart
amPie(data = data_pie, inner_radius = 50)

#all
amPie(data = data_pie, inner_radius = 50, depth = 10, show_values = FALSE)

