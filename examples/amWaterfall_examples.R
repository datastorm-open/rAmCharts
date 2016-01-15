# Basic example
data_waterfall <- data.frame(label = c("Income 1", "Income 2", "Income 3", "Total 1", 
                                       "Expenses 1", "Expenses 2", "Total 2", "Income 4", 
                                       "Income 5", "Income 6", "Expenses 3","Total 3", 
                                       "Expenses 4", "Expenses 5", "Total 4"),
                             value = c(5, 10, 15, 30, 10, 5, 15, 50, 10, 35, 10, 100, 
                                        15, 60, 25),
                             operation = c(rep("plus", 3), "total", rep("minus", 2),
                                            "total", "plus", "minus", rep("plus", 2), 
                                            "total", rep("minus", 2), "total"), 
                                            stringsAsFactors = FALSE)

#simple exemple
amWaterfall(data = data_waterfall, show_values = TRUE)



#change the orientation :
amWaterfall(data = data_waterfall, horiz = TRUE)                                  
