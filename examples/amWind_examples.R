##data
wind <- data.frame(Week = c(1, 2, 3, 4,1, 2,1, 2),
                   Middle = c(2, 8, 1, 1,2, 8,1, 2),
                   Strong = c(1, 1, 2, 2,1, 1 ,1, 2))
#Basic
amWind(wind)


#Color
amWind(wind, col = "#0404B4")
amWind(wind, col = c("#0404B4","#01DF01","#FFBF00"))

#backTransparency
amWind(wind, col = c("#0404B4","#01DF01","#FFBF00"), backTransparency = 0.1)
amWind(wind, col = c("#0404B4","#01DF01","#FFBF00"), backTransparency = 1)
amWind(wind, col = c("#0404B4","#01DF01","#FFBF00"), backTransparency = c(0.1, 0.1, 1))


