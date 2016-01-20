require(pipeR)
#Data
data("data_radar")

#Basic
amRadar(data_radar)

#col
amRadar(data_radar, col = "#FF0000")
amRadar(data_radar, col = c("#0000FF", "#00FF00", "#FF0000"))


#backTransparency
amRadar(data_radar, backTransparency = 0.6)
amRadar(data_radar, backTransparency = c(0, 0.4, 0.6))


#type
amRadar(data_radar, type = "circles")

#pch
amRadar(data_radar,  pch = "triangleRight")
amRadar(data_radar,  pch = "triangleLeft")


