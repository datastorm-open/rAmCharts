require(rAmCharts)
require(pipeR)
data(data_gdp)
a <- amPieChart(valueField = 'gdp', titleField = 'country',
                dataProvider = data_gdp, startDuration = 0) %>>%
  setExport() %>>%
  setLegend(position = "bottom", useMarkerColorForLabels = TRUE) %>>%
  addListener(name = "clickSlice", expression = paste("function (event) {",
                                                      "var obj = event.dataItem;",
                                                      "alert('The value is: ' + obj.value);",
                                                      "}")) %>>%
  addListener(name = "clickSlice99", expression = paste("function (event) {",
                                                      "var obj = event.dataItem;",
                                                      "alert('The value is: ' + obj.value);",
                                                      "}"))
plot(a)
str(a)

test <- c("foo", "ooo", "listeners")
grep(pattern = "^listeners", x = test)
