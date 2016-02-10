require(pipeR)

# This has been created to list the problem encountered with 'amOptions'

# problem with export options
amAngularGauge(x = 25) %>>% setExport()
amAngularGauge(x = 25, exportFormat = "CSV") # impossible
