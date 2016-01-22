stopifnot(require(pipeR))

#Basic Example : column chart
data(data_fbar)

# test with label rotation
amFloatingBar(x = "country", y_inf = "visits_inf", y_sup = "visits_sup",
              data = data_fbar, labelRotation = -90)

# horizontal bar
amFloatingBar(x = "country", y_inf = "visits_inf", y_sup = "visits_sup",
              data = data_fbar, labelRotation = -90, horiz = TRUE)

#3D bar
amFloatingBar(x = "country", y_inf = "visits_inf", y_sup = "visits_sup",
              data = data_fbar, labelRotation = -90, depth = 15)

#display values
amFloatingBar(x = "country", y_inf = "visits_inf", y_sup = "visits_sup",
              data = data_fbar, labelRotation = -90, show_values = TRUE)

#grouped columns
data(data_gbar)

# Parse dates

# default label: firt day of each year

  amFloatingBar(x = "year", y_inf = "expenses", y_sup = "income", data = data_gbar,
                dataDateFormat = "YYYY", minPeriod = "YYYY", zoom = TRUE)

# default label: first day of each month

  amFloatingBar(x = "month", y_inf = "expenses", y_sup = "income", data = data_gbar,
                dataDateFormat = "MM/YYYY", minPeriod = "MM", zoom = TRUE)


  amFloatingBar(x = "day", y_inf = "expenses", y_sup = "income", data = data_gbar,
                dataDateFormat = "DD/MM/YYYY", zoom = TRUE)

