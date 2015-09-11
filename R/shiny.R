#' @title Few shiny examples
#' @description Launch a shiny app with amChart examples
#' @examples
#' \donttest{
#' runShinyExamples()
#' }
#' @import data.table
#' @export
runShinyExamples <- function()
{
  if (!requireNamespace("pipeR")) {
    stop("Please install pipeR for running this function")
  } else {}
  if (!requireNamespace("shiny")) {
    stop("Please install shiny for running this function")
  } else {}
  
  # Load App
  shiny::shinyApp(
    ui <- shiny::fluidPage(
      # Application title
      shiny::titlePanel("Charts by rAmCharts"),
      
      shiny::fluidRow(
        shiny::column(
          width = 6,
          amChartsOutput("radar", type = "radar"),
          amChartsOutput("radar2", type = "radar"),
          amChartsOutput("pie", type = "pie")
        ),
        shiny::column(
          width = 6,
          amChartsOutput("serial", type = "serial"),
          amChartsOutput("drillColumnChart1", type = "drill"),
          amChartsOutput("drillColumnChart2", type = "drill")
        )
      )
    ),
    server <- function(input, output) {
      
      category <- shiny::reactive({
        "attribute"
      })
      
      output$radar <- renderAmCharts({
        pipeR::pipeline(
          amRadarChart(startDuration = 1, categoryField = "attribute", theme = "dark"),
          setDataProvider(data.frame(attribute = c("data", "brand", "singleness"),
                                     p1 = c(.3, -1, 0), p2 = c(.7, 1, 2))),
          addGraph(balloonText = "Utility : [[value]]", valueField = "p1", title = "p1"),
          addGraph(balloonText = "Utility : [[value]]", valueField = "p2", title = "p2"),
          setLegend(useGraphSettings = TRUE),
          setExport(),
          plot()
        )
      })
      
      output$radar2 <- renderAmCharts({
        pipeR::pipeline(
          amRadarChart(startDuration = 1, categoryField = "attribute",
                       theme = "chalk", creditsPosition = "bottom-right"),
          setDataProvider(data.table(attribute = c("data", "brand", "singleness"),
                                     p1 = c(.3, -1, 0), p2 = c(.7, 1, 2))),
          addGraph(balloonText = "Utility : [[value]]", valueField = "p1", title = "p1"),
          addGraph(balloonText = "Utility : [[value]]", valueField = "p2", title = "p2"),
          setLegend(useGraphSettings = TRUE),
          setExport(), plot()
        )
      })
      
      output$pie <- renderAmCharts({
        pipeR::pipeline(
          amPieChart(valueField = "value", titleField = "country", creditsPostion = "top-right"),
          setDataProvider(data.table(country = c("FR", "US"), value = c(20,10))),
          setExport(), plot()
        )
      })
      
      output$serial <- renderAmCharts({
        pipeR::pipeline(
          amSerialChart(categoryField = "country", creditsPosition = "top-right", theme = "light"),
          setDataProvider(data.table(country = c("FR", "US"), visits = 1:2)),
          addGraph(balloonText = "[[category]]: <b>[[value]]</b>", type = "column",
                   valueField = "visits", fillAlphas = 0.8, lineAlpha = 0.2),
          setExport(), plot()
        )
      })
      
      output$drillColumnChart1 <-rAmCharts::renderAmCharts({
        pipeR::pipeline(
          amSerialChart(categoryField = "name"),
          setDataProvider(data.table(name = c("data", "Brand", "singleness"),
                                     start = c(8,10,6), end = c(11,13,10),
                                     color = c('#007FFF', "#007FFF", "#003FFF"),
                                     description = c("click to drill-down","",""))),
          addGraph(valueField = "end", type = "column", openField = "start", lineAlpha = 0,
                   fillColorsField = "color", fillAlphas = 0.9,
                   balloonText = "<b>[[category]]</b><br>from [[start]] to [[end]]<br>[[description]]"),
          addSubData(1, data.table(modality = c("3G", "4G"), utility = c(-1,2), 
                                   color = c("#007FFF", "#007FFF"))),
          setSubChartProperties(.subObject = amSerialChart(creditsPosition = "bottom-right", categoryField = "modality")),
          addGraph(valueField = 'utility', type = 'column', categoryField = "modality",
                   lineAlpha = 0, fillColorsField = "color", fillAlphas = 0.9,
                   balloonText = "[[modality]]: <b>[[utility]]</b>"),
          setExport(), plot()
        )
      })
      
      output$drillColumnChart2 <-rAmCharts::renderAmCharts({
        pipeR::pipeline(
          amSerialChart(categoryField = "name"),
          setDataProvider(data.table(name = c("data", "Brand", "singleness"),
                                     start = c(8,10,6), end = c(11,13,10),
                                     color = c('#007FFF', "#007FFF", "#003FFF"),
                                     description = c("click to drill-down","",""))),
          addGraph(valueField = "end", type = "column", openField = "start", lineAlpha = 0,
                   fillColorsField = "color", fillAlphas = 0.9,
                   balloonText = "<b>[[category]]</b><br>from [[start]] to [[end]]<br>[[description]]"),
          addSubData(1, data.table(modality = c("3G", "4G"), utility = c(-1,2), 
                                   color = c("#007FFF", "#007FFF"))),
          setSubChartProperties(.subObject = amSerialChart(creditsPosition = "bottom-right", categoryField = "modality")),
          addGraph(valueField = 'utility', type = 'column', categoryField = "modality",
                   lineAlpha = 0, fillColorsField = "color", fillAlphas = 0.9,
                   balloonText = "[[modality]]: <b>[[utility]]</b>"),
          setExport(), plot()
        )
      }) 
    }
  )
}