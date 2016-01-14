require(pipeR)

data <- data.frame(label = c("A", "Z", "E", "R", "T"),
                   Product1 = c(1, 2, 3, 4, 2), 
                   Product2 = c(2, 8, 1, 1, 0),
                   Product3 = c(1,1,2,2,4))

#Basic
amRadar(data)

#col
amRadar(data, col = "#FF0000")
amRadar(data, col = c("#0000FF", "#00FF00", "#FF0000"))


#backTransparency
amRadar(data, backTransparency = 0.6)
amRadar(data, backTransparency = c(0, 0.4, 0.6))


#type
amRadar(data, type = "circles")

#pch
amRadar(data,  pch = "triangleRight")
amRadar(data,  pch = "triangleLeft")


