stopifnot(require(pipeR))

# Load data
data(data_fbar)

# Reference example : column chart
amFloatingBar(x = "country", y_inf = "visits_inf", y_sup = "visits_sup",
              data = data_fbar, labelRotation = -45)

# Label rotation modification
amFloatingBar(x = "country", y_inf = "visits_inf", y_sup = "visits_sup",
              data = data_fbar, labelRotation = -90)

# Horizontal bar
amFloatingBar(x = "country", y_inf = "visits_inf", y_sup = "visits_sup",
              data = data_fbar, horiz = TRUE)

# 3D bar
amFloatingBar(x = "country", y_inf = "visits_inf", y_sup = "visits_sup",
              data = data_fbar, labelRotation = -45, depth = 15)

# Display values
amFloatingBar(x = "country", y_inf = "visits_inf", y_sup = "visits_sup",
              data = data_fbar, labelRotation = -90, show_values = TRUE)

# Change colors
amFloatingBar(x = "country", y_inf = "visits_inf", y_sup = "visits_sup",
              data = data_fbar[,1:3], labelRotation = -45, groups_color = "#67b7dc")


# Grouped columns
data(data_gbar)

# Parse dates

# Default label: firt day of each year

  amFloatingBar(x = "year", y_inf = "expenses", y_sup = "income", data = data_gbar,
                dataDateFormat = "YYYY", minPeriod = "YYYY", zoom = TRUE)

# Default label: first day of each month

  amFloatingBar(x = "month", y_inf = "expenses", y_sup = "income", data = data_gbar,
                dataDateFormat = "MM/YYYY", minPeriod = "MM", zoom = TRUE)


  amFloatingBar(x = "day", y_inf = "expenses", y_sup = "income", data = data_gbar,
                dataDateFormat = "DD/MM/YYYY", zoom = TRUE)

