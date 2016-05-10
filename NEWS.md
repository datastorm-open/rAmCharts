## LOG CHANGE

### rAmCharts 2.0.1 (based on [amcharts][amcharts_url] version **3.18.3**)

  * add ``getAmChart()`` javascript function to use and update chart within shiny
  * fix ``runExamples()`` bug
  * add ``dataLoader`` control for stock
  
### rAmCharts 2.0.0 (based on [amcharts][amcharts_url] version **3.18.3**)

  * New version available on CRAN, 
  * New functions 'am'
  * New shiny application : ``runExamples()``
  * New API

### rAmCharts 1.1.3 (based on [amcharts][amcharts_url] version **3.18.3**)

  * fix bug in markdown : now a call to method 'plot' is optionnal
  * New functions 'am'
  * fix init and rendered event with shiny
  * fix class 'Label' to allow character for properties 'x' and 'y'
  * Improve `am___` functions (histogram, scatter plot, gauge, funnel, etc.)

### rAmCharts 1.1.2 (based on [amcharts][amcharts_url] version **3.17.2**)
  
  * argument type in `amChartsOutput` is required only when `type = drill`
  * call to `plot` in shiny no longer necessary
  * better management of dependencies (dynamic loading in __R__)
  
[amcharts_url]: http://www.amcharts.com

---

### rAmCharts 1.1.1
  
  * Create a generic method for renderAmCharts (shiny use)
  * Create a show method for class AmChart (now plot the object if type is set)
  * fix bug in dependencies for html_document
  * Clean import when creating widget (plot method)
  * fix bug using renderAmChart with input/reactive values
  * rename renderAmChart -> renderAmCharts
  * rename amChartOutput -> amChartsOutput
  * add listeners on legend
  * new function 'runShinyExamples'
  * access the documentation with the functions api() or api(class = [className])
  
---

