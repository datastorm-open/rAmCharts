
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rAmCharts)
library(pipeR)

shinyServer(function(input, output) {
  
  category <- reactive({
    "attribute"
  })
  
  output$radar <- rAmCharts::renderAmCharts({
    amRadarChart( 
      startDuration = 1, categoryField = category(), theme = "dark"
    ) %>>% setDataProvider(
      data.frame(
        attribute = c("data", "brand", "singleness"), p1 = c(.3, -1, 0), p2 = c(.7, 1, 2)
      )
    ) %>>% addGraph( balloonText = "Utility : [[value]]", valueField = "p1", title = "p1"
    ) %>>% addGraph( balloonText = "Utility : [[value]]", valueField = "p2", title = "p2"
    ) %>>% setLegend( useGraphSettings = TRUE
    ) %>>% setExport() %>>% plot  
  })
  
  output$radar2 <- rAmCharts::renderAmCharts({
    amRadarChart( 
      startDuration = 1, categoryField = "attribute", theme = "chalk", creditsPosition = "bottom-right"
    ) %>>% setDataProvider(
      data.frame(
        attribute = c("data", "brand", "singleness"), p1 = c(.3, -1, 0), p2 = c(.7, 1, 2)
      )
    ) %>>% addGraph( balloonText = "Utility : [[value]]", valueField = "p1", title = "p1"
    ) %>>% addGraph( balloonText = "Utility : [[value]]", valueField = "p2", title = "p2"
    ) %>>% setLegend( useGraphSettings = TRUE
    ) %>>% setExport() %>>% plot  
  })
  
  output$pie <- rAmCharts::renderAmCharts({
    amPieChart(valueField = "value", titleField = "key", creditsPostion = "top-right"
    ) %>>% setDataProvider(data.frame(key = c("FR", "US"), value = c(20,10))
    ) %>>% setExport() %>>% plot
  })
  
  output$serial <- rAmCharts::renderAmCharts({
    amSerialChart( categoryField = "country", creditsPosition = "top-right", theme = "light"
    ) %>>% setDataProvider(data.frame(country = c("FR", "US"), visits = 1:2)
    ) %>>% addGraph( balloonText = "[[category]]: <b>[[value]]</b>", type = "column",
                     valueField = "visits", fillAlphas = 0.8, lineAlpha = 0.2
    ) %>>% setExport() %>>% plot
  })
  
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
})
