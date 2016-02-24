library(pipeR)

# Load data
data(data_mekko)

# Reference example : mekko chart
amMekko(x = "var1", y = "var2", data = data_mekko)

# Horizontal
amMekko(x = "var1", y = "var2", data = data_mekko, horiz = TRUE)

# Display values
amMekko(x = "var1", y = "var2", data = data_mekko, show_values = TRUE)%>>%amOptions()

