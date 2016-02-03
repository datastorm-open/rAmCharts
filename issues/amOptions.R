require(pipeR)
# problem with export options
amAngularGauge(x = 25) %>>% setExport()
amAngularGauge(x = 25, exportFormat = "SVG")
data("data_candleStick2")
amCandlestick(data = data_candleStick2, horiz = TRUE, exportFormat = "CSV") %>>%
  setProperties(precision = 5)
