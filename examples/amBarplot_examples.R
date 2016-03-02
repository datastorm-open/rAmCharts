stopifnot(require(pipeR))

# Data
data(data_bar)


# Test with label rotation
amBarplot(x = "country", y = "visits", data = data_bar,
          labelRotation = -45) 

# Horizontal bar
amBarplot(x = "country", y = "visits", data = data_bar, horiz = TRUE, labelRotation = -45)

# 3D bar
amBarplot(x = "country", y = "visits", data = data_bar, depth = 15, labelRotation = -45)

# Display values
amBarplot(x = "country", y = "visits", data = data_bar, show_values = TRUE, labelRotation = -45)

# Grouped columns
data(data_gbar)

amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar)

# Parse dates

# Default label: first day of each year
pipeR::pipeline(
  amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar,
            dataDateFormat = "YYYY", minPeriod = "YYYY"),
  setChartCursor()
)

# Default label: first day of each month
pipeR::pipeline(
  amBarplot(x = "month", y = c("income", "expenses"), data = data_gbar,
            dataDateFormat = "MM/YYYY", minPeriod = "MM"),
  setChartCursor()
)

pipeR::pipeline(
  amBarplot(x = "day", y = c("income", "expenses"), data = data_gbar,
            dataDateFormat = "DD/MM/YYYY"),
  setChartCursor()
)

# Change groups colors
amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, 
      groups_color = c("#87cefa", "#c7158"))

# Stacked bars
amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, stack_type = "regular")

# 100% stacked bars
amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, stack_type = "100")

# Layered bars
amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, layered = TRUE)

# Data with row names
dataset <- data.frame(get(x = "USArrests", pos = "package:datasets"))
amBarplot(y = c("Murder", "Assault", "UrbanPop", "Rape"), data = dataset, stack_type = "regular")


# Round values
amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar) %>>%
  setProperties(precision = 0)
