stopifnot(require(pipeR))

#Basic Example : column chart
data_fbar <- data.frame(country = c("USA", "China", "Japan", "Germany", 
                                   "UK", "France", "India", "Spain",
                                   "Netherlands", "Russia", "South Korea",
                                   "Canada"),
                       visits_inf = c(3000, 1800, 1000, 1300, 1100, 1000, 
                                  900, 0, 600, 0, 400, 0),
                       visits_sup = c(3025, 1882, 1809, 1322, 1122, 1114, 
                                  984, 711, 665, 580, 443, 441),
                       color = c("#FF0F00", "#FF6600", "#FF9E01", "#FCD202",
                                 "#F8FF01", "#B0DE09", "#04D215", "#0D8ECF",
                                 "#0D52D1", "#2A0CD0", "#8A0CCF", "#CD0D74"),
                       stringsAsFactors = FALSE)

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

