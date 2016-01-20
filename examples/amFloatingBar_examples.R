stopifnot(require(pipeR))

#Basic Example : column chart
data(data_fbar)

# test with label rotation
amFloatingBar(x = "country", y_inf = "visits_inf", y_sup = "visits_sup", data = data_fbar, labelRotation = -90)

#horizontal bar
amFloatingBar(x = "country", y_inf = "visits_inf", y_sup = "visits_sup", data = data_fbar, labelRotation = -90,
              horiz = TRUE)

#3D bar
amFloatingBar(x = "country", y_inf = "visits_inf", y_sup = "visits_sup", data = data_fbar, labelRotation = -90,
              depth = 15)

#display values
amFloatingBar(x = "country", y_inf = "visits_inf", y_sup = "visits_sup", data = data_fbar, labelRotation = -90,
              show_values = TRUE)

#grouped columns
data_gbar <- data.frame(year = c("2005", "2006", "2007", "2008", "2009"),
                        day = c("01/06/2005", "02/06/2005", "03/06/2005",
                                "04/06/2005", "05/06/2005"),
                        month = c("06/2005", "07/2005", "08/2005",
                                  "09/2005", "10/2005"),
                        income = c(23.5, 26.2, 30.1, 29.5, 24.6),
                        expenses = c(18.1, 22.8, 23.9, 25.1, 25),
                        stringsAsFactors = FALSE)

# Parse dates

# default label: firt day of each year
pipeR::pipeline(
  amFloatingBar(x = "year", y_inf = "expenses", y_sup = "income", data = data_gbar,
            dataDateFormat = "YYYY", minPeriod = "YYYY"),
  setChartCursor()
)

# default label: first day of each month
pipeR::pipeline(
  amFloatingBar(x = "month", y_inf = "expenses", y_sup = "income", data = data_gbar,
            dataDateFormat = "MM/YYYY", minPeriod = "MM"),
  setChartCursor()
)

pipeR::pipeline(
  amFloatingBar(x = "day", y_inf = "expenses", y_sup = "income", data = data_gbar,
            dataDateFormat = "DD/MM/YYYY"),
  setChartCursor()
)

