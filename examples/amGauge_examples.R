require(pipeR)

# Reference example : solid gauge
amSolidGauge(x = 65)

# Change min and max values
amSolidGauge(x = 65, min = 0, max = 200)

# Semi solid gauge
amSolidGauge(x = 65, type = "semi")

# Change width
amSolidGauge(x = 65, width = 50)

# Change color
amSolidGauge(x = 65, color = "#2F4F4F")

# Put a color scale
amSolidGauge(x = 10, color = c("#00ff00", "#ffd700", "#ff0000"))
amSolidGauge(x = 35, color = c("#00ff00", "#ffd700", "#ff0000"))
amSolidGauge(x = 70, color = c("#00ff00", "#ffd700", "#ff0000"))
amSolidGauge(x = 90, color = c("#00ff00", "#ffd700", "#ff0000"))

# Add some text to the printed value
amSolidGauge(x = 65, text = "%")
  
# Modify textSize value
amSolidGauge(x = 65, text = "%", textSize = 50)
