#' @title Few shiny examples
#' @description Launch a shiny app with amChart examples
#' @examples
#' \donttest{
#' library(pipeR)
#' library(shiny)
#' shinyExamples()
#' }
#' @export
shinyExamples <- function()
{
  # Check package dependencies
  stopifnot(require(shiny))
  stopifnot(require(pipeR))
  
  # Load App
  shinyApp(
    ui <- fluidPage(
      # Application title
      titlePanel("Charts by rAmCharts"),
      
      fluidRow(
        column(
          width = 6,
          rAmCharts::amChartsOutput("radar", type = "radar"),
          rAmCharts::amChartsOutput("radar2", type = "radar"),
          rAmCharts::amChartsOutput("pie", type = "pie")
        ),
        column(
          width = 6,
          rAmCharts::amChartsOutput("serial", type = "serial"),
          rAmCharts::amChartsOutput("drillColumnChart1", type = "drill"),
          rAmCharts::amChartsOutput("drillColumnChart2", type = "drill")
        )
      )
    ),
    server <- function(input, output) {
      
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
    }
  )
}