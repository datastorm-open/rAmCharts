# Load data
data("data_wind")

# Reference example : wind chart 
amWind(data_wind)

# Change color
amWind(data = data_wind, col = "#0404B4")
amWind(data = data_wind, col = c("#0404B4","#01DF01","#FFBF00"))

# Change backTransparency
amWind(data = data_wind, col = c("#0404B4","#01DF01","#FFBF00"), backTransparency = 0.1)
amWind(data = data_wind, col = c("#0404B4","#01DF01","#FFBF00"), backTransparency = 1)
amWind(data = data_wind, col = c("#0404B4","#01DF01","#FFBF00"), backTransparency = c(0.1, 0.1, 1))


