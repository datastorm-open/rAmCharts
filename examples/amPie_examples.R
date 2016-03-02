# Load data
data("data_pie")

# Reference example : pie chart                
amPie(data = data_pie)

# Don't display values
amPie(data = data_pie, show_values = FALSE)

# 3D pie
amPie(data = data_pie, depth = 10)

# Donut chart
amPie(data = data_pie, inner_radius = 50)

# All parameters
amPie(data = data_pie, inner_radius = 50, depth = 10, show_values = FALSE)

