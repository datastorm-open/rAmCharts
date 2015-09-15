output$drillColumnChart1 <-rAmCharts::renderAmCharts({
  df <- data.frame(
    name = c("data", "Brand", "singleness"), start = c(8,10,6), end = c(11,13,10),
    color = c('#007FFF', "#007FFF", "#003FFF"), description = c("click to drill-down","","")
  )
  
  amSerialChart( categoryField = "name", dataProvider = df
  ) %>>% addGraph(valueField = "end", type = "column", openField = "start", lineAlpha = 0,
                  fillColorsField = "color", fillAlphas = 0.9,
                  balloonText = "<b>[[category]]</b><br>from [[start]] to [[end]]<br>[[description]]"
  ) %>>% addSubData( 1, data.frame( modality = c("3G", "4G"), utility = c(-1,2), 
                                    color = c("#007FFF", "#007FFF") )
  ) %>>% setSubChartProperties(.subObject = amSerialChart(creditsPosition = "bottom-right", categoryField = "modality"
  ) %>>% addGraph(valueField = 'utility', type = 'column', categoryField = "modality",
                  lineAlpha = 0, fillColorsField = "color", fillAlphas = 0.9,
                  balloonText = "[[modality]]: <b>[[utility]]</b>")
  ) %>>% setExport() %>>% plot
})

output$drillColumnChart2 <-rAmCharts::renderAmCharts({
  amSerialChart(categoryField = "name", theme = "chalk"
  ) %>>%setDataProvider( data.frame( name = c("data", "Brand", "singleness"),
                                     start = c(8,10,6), end = c(11,13,10),
                                     color = c('#007FFF', "#007FFF", "#003FFF"),
                                     description = c("click to drill-down","","") )
  ) %>>% addGraph( valueField = "end", type = "column", openField = "start", lineAlpha = 0,
                   fillColorsField = "color", fillAlphas = 0.9,
                   balloonText = "<b>[[category]]</b><br>from [[start]] to [[end]]<br>[[description]]"
  ) %>>% addSubData( 1, data.frame( modality = c("3G", "4G"), utility = c(-1,2), 
                                    color = c("#007FFF", "#007FFF") )
  ) %>>% setSubChartProperties(.subObject = amSerialChart(creditsPosition = "bottom-right", categoryField = "modality"
  ) %>>% addGraph( valueField = 'utility', type = 'column', categoryField = "modality",
                   lineAlpha = 0, fillColorsField = "color", fillAlphas = 0.9,
                   balloonText = "[[modality]]: <b>[[utility]]</b>" )
  ) %>>% setExport() %>>% plot
})