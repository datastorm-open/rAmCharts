library(pipeR)
data(data_mekko)
amMekko(x = "var1", y = "var2", data = data_mekko)

#Horizontal :
amMekko(x = "var1", y = "var2", data = data_mekko, horiz = TRUE)

#display values :
amMekko(x = "var1", y = "var2", data = data_mekko, show_values = TRUE)%>>%amOptions()

