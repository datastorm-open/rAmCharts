#Basic Example : column chart
data_bar <- data.frame(country = c("USA", "China", "Japan", "Germany", 
                                   "UK", "France", "India", "Spain",
                                   "Netherlands", "Russia", "South Korea",
                                   "Canada"),
                       visits = c(3025, 1882, 1809, 1322, 1122, 1114, 
                                  984, 711, 665, 580, 443, 441),
                       color = c("#FF0F00", "#FF6600", "#FF9E01", "#FCD202",
                                 "#F8FF01", "#B0DE09", "#04D215", "#0D8ECF",
                                 "#0D52D1", "#2A0CD0", "#8A0CCF", "#CD0D74"),
                       stringsAsFactors = FALSE)

amBarplot(x = "country", y = "visits", data = data_bar)

#horizontal bar
amBarplot(x = "country", y = "visits", data = data_bar, horiz = TRUE)

#3D bar
amBarplot(x = "country", y = "visits", data = data_bar, third_dim = TRUE)

#display values
amBarplot(x = "country", y = "visits", data = data_bar, show_values = TRUE)

#grouped columns
data_gbar <- data.frame(year = c("2005", "2006", "2007", "2008", "2009"),
                        income = c(23.5, 26.2, 30.1, 29.5, 24.6),
                        expenses = c(18.1, 22.8, 23.9, 25.1, 25),
                        stringsAsFactors = FALSE)

amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar)

#add legend
amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, legend = TRUE)

#change groups colors
amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, 
      groups_color = c("#87cefa", "#c7158"), legend = TRUE)

#stacked bars
amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, stack_type = "regular")

#100% stacked bars
amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, stack_type = "100")

#layered bars
amBarplot(x = "year", y = c("income", "expenses"), data = data_gbar, layered = TRUE)

#data with row names
dataset <- data.frame(get(x = "USArrests", pos = "package:datasets"))
amBarplot(y = c("Murder", "Assault", "UrbanPop", "Rape"), data = dataset, stack_type = "regular")
