library(rAmCharts)
library(shiny)
library(pipeR)
library(data.table)
shinyApp(
  ui = fluidPage(
    amChartsOutput(outputId = "chart"),
    fluidRow(
      column(width = 1, strong("Values: ")),
      column(width = 3, verbatimTextOutput("result"))
    ),
    br(), hr(), br(),
    amChartsOutput(outputId = "serial10")
  ),
  
  server = function(input, output) {
    output$chart <- renderAmCharts({
      # prepare data
      dp <- data.table(name = paste0('foo', 1:5),
                       income = round(rnorm(5, mean = 30000, sd = 2000)))
      # build the chart
      pipeline(
        amSerialChart(categoryField = 'name', dataProvider = dp),
        addGraph(type = 'column', valueField = "income", fillAlphas = .6),
        addListener(name = 'clickGraphItem',
                    expression = paste('function(event){',
                                       'Shiny.onInputChange(\'myValues\', event.item.values);',
                                       '}'))
      )
    })
    output$result <- renderPrint({
      input$myValues
    })
    
    output$serial10 <- renderAmCharts({
      start <- as.POSIXct('01-01-2015', format = '%d-%m-%Y')
      end <- as.POSIXct('31-12-2015', format = '%d-%m-%Y')
      date <- seq.POSIXt(from = start, to = end, by = 'day')
      date <- format(date, '%m-%d-%Y')
      low <- c() ; open <- c() ; close <- c() ; high <- c() ; median <- c()
      
      n <- 100
      invisible(
        sapply(1:length(date), function(i)
        {
          sample <- rnorm(n, mean = sample(1:10, 1), sd = sample(1:10/10, 1))
          quant <- boxplot(sample, plot = FALSE)$stats
          low <<- c(low, quant[1])
          open <<- c(open, quant[2])
          median <<- c(median, quant[3])
          close <<- c(close, quant[4])
          high <<- c(high, quant[5])
        })
      )
      dp <- data.table(date = date, low = round(low, 2), open = round(open, 2), 
                       close = round(close, 2), high = round(high, 2), median = round(median, 2)) 
      balloonText <- paste('Open:<b>[[open]]</b><br>',
                           'Low:<b>[[low]]</b><br>',
                           'High:<b>[[high]]</b><br>',
                           'Close:<b>[[close]]</b><br>')
      pipeline(
        amSerialChart(categoryField = 'date', dataDateFormat = 'MM-DD-YYYY',
                      dataProvider = dp, startDuration = 0),
        setCategoryAxis(parseDates = TRUE),
        addGraph(id = 'g1', type = 'candlestick', lineColor = '#7f8da9',
                 lowField = 'low', closeField = 'close',
                 highField = 'high', openField = 'open', valueField = 'median',
                 balloonText = balloonText,
                 lineColor = '#7f8da9', lineAlpha = 1, fillAlphas = 0.9, negativeBase = 5,
                 negativeFillColors = '#db4c3c', negativeLineColor = '#db4c3c',
                 title = 'Price: '),
        setChartCursor(valueLineEnabled = TRUE, valueLineBalloonEnabled = TRUE),
        setChartScrollbar(graph = 'g1', graphType = 'line'),
        addListener("init", paste('function(event) {',
                                      'alert(\'rendered\');',
                                      'console.log(event);',
                                      # 'var nbCandles = event.chart.dataProvider.length;',
                                      # 'event.chart.zoomToIndexes(20, 100);',
                                      ' }')),
        addListener("rightClickGraphItem", paste('function(event) {alert(\'rightClickGraphItem\');}'))
      )
    })
  }
)