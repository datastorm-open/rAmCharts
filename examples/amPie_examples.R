#Data
data("data_pie")

#Basic example                 
amPie(data = data_pie)


#don't display values
amPie(data = data_pie, show_values = FALSE)

#3D pie
amPie(data = data_pie, depth = 10)

#donut chart
amPie(data = data_pie, inner_radius = 50)

#all
amPie(data = data_pie, inner_radius = 50, depth = 10, show_values = FALSE)

