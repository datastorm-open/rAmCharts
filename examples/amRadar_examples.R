require(pipeR)

# Load data
data("data_radar")

# Reference example : radar chart 
amRadar(data_radar, labelRotation = 90)

# Change color
amRadar(data_radar, col = "#FF0000")
amRadar(data_radar, col = c("#0000FF", "#00FF00", "#FF0000"))


# Change backTransparency
amRadar(data_radar, backTransparency = 0.6)
amRadar(data_radar, backTransparency = c(0, 0.4, 0.6))


# Change type
amRadar(data_radar, type = "circles")

# Change pch
amRadar(data_radar,  pch = "triangleRight")
amRadar(data_radar,  pch = "triangleLeft")

