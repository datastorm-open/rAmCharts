library(shiny)
library(rAmCharts)
library(pipeR)
library(shinyjs)

jsCode <- "shinyjs.updateTheme = function(theme) {alert(instance.amchart);}"

shinyApp(
  ui = fluidPage(
    useShinyjs(),
    extendShinyjs(text = jsCode),
    selectInput(inputId = "theme", label = "theme", choices = c("light", "patterns")),
    amChartsOutput(outputId = "chart")),
  server = function (input, output, session) {
    observeEvent(input$theme, {
      js$updateTheme(input$theme)
    })

    output$chart <- renderAmCharts({
      data('data_stock1')
      pipeR::pipeline(
        amStockChart(startDuration = 0),
        addDataSet(
          pipeR::pipeline(dataSet(title = 'first data set', categoryField = 'date',
                                  dataProvider = data_stock1$chartData1),
                          addFieldMapping(fromField = 'value', toField = 'value'),
                          addFieldMapping(fromField = 'volume', toField = 'volume'))
        ),
        addDataSet(
          pipeR::pipeline(dataSet(title = 'second data set', categoryField = 'date',
                                  dataProvider = data_stock1$chartData2),
                          addFieldMapping(fromField = 'value', toField = 'value'),
                          addFieldMapping(fromField = 'volume', toField = 'volume'))
        ),
        addPanel(
          pipeR::pipeline(
            stockPanel(showCategoryAxis = FALSE, title = 'Value', percentHeight = 70),
            addStockGraph(id = 'g1', valueField = 'value', comparable = TRUE,
                          compareField = 'value', balloonText = '[[title]] =<b>[[value]]</b>',
                          compareGraphBalloonText = '[[title]] =<b>[[value]]</b>'),
            setStockLegend(periodValueTextComparing = '[[percents.value.close]]%',
                           periodValueTextRegular = '[[value.close]]'))
        ),
        addPanel(
          pipeR::pipeline(stockPanel(title = 'Volume', percentHeight = 30),
                          addStockGraph(valueField = 'volume', type = 'column', fillAlphas = 1),
                          setStockLegend(periodValueTextRegular = '[[value.close]]'))
        ),
        setChartScrollbarSettings(graph = 'g1'),
        setChartCursorSettings(valueBalloonsEnabled = TRUE, fullWidth = TRUE,
                               cursorAlpha = 0.1, valueLineBalloonEnabled = TRUE,
                               valueLineEnabled = TRUE, valueLineAlpha = 0.5),
        setPeriodSelector(
          pipeR::pipeline(periodSelector(position = 'left'),
                          addPeriod(period = 'DD', selected = TRUE, count = 7, label = '1 week'),
                          addPeriod(period = 'MAX', label = 'MAX'))
        ),
        setDataSetSelector(position = 'left'),
        setPanelsSettings(recalculateToPercents = FALSE)
      )
    })
  }  
)