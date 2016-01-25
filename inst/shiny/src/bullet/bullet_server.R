output$Bullet0 <- rAmCharts::renderAmCharts({
##Data
rates <- data.frame(name = c('excelent', 'good', 'average', 'poor', 'bad'),
                    min = c(0, 20, 40, 60, 80),
                    max = c(20, 40, 60, 80, 100),
                    color = c('#19d228', '#b4dd1e', '#f4fb16',
                              '#f6d32b', '#fb7116'),
                    stringsAsFactors = FALSE)
dataProvider <- data.frame(category = '', t(rates$max - rates$min), stringsAsFactors = FALSE)
colnames(dataProvider)[-1] <- as.character(rates$name)
dataProvider$limit <- 90
dataProvider$full <- 100
dataProvider$bullet <- 85
##Plot
chart <-   sapply(1:nrow(rates), FUN = function(rt) {
amGraph(type = 'column', valueField = as.character(rates$name[rt]), fillAlphas = 0.8,
                     lineColor = rates$color[rt], showBalloon = FALSE, columnWidth = 1)
})
chart <- c(chart,amGraph(type = 'step', valueField = 'limit', columnWidth = 0.5, 
         lineThickness = 3, noStepRisers = TRUE, stackable = FALSE))
chart <- c(chart,amGraph(type = 'column', valueField = 'bullet', columnWidth = 0.3, fillAlphas = 1, clustered = FALSE, stackable = FALSE))
pipeR::pipeline(
amSerialChart(dataProvider = dataProvider , categoryField = 'category',
               columnWidth = 1) ,
  addValueAxes(stackType = 'regular', gridAlpha = 0),
  setGraphs(chart)
)
})

output$code_Bullet0 <- renderText({
  "
  ##Data
  rates <- data.frame(name = c('excelent', 'good', 'average', 'poor', 'bad'),
                      min = c(0, 20, 40, 60, 80),
                      max = c(20, 40, 60, 80, 100),
                      color = c('#19d228', '#b4dd1e', '#f4fb16',
                                '#f6d32b', '#fb7116'),
                      stringsAsFactors = FALSE)
  dataProvider <- data.frame(category = '', t(rates$max - rates$min), stringsAsFactors = FALSE)
  colnames(dataProvider)[-1] <- as.character(rates$name)
  dataProvider$limit <- 90
  dataProvider$full <- 100
  dataProvider$bullet <- 85
  ##Plot
  chart <-   sapply(1:nrow(rates), FUN = function(rt) {
    amGraph(type = 'column', valueField = as.character(rates$name[rt]), fillAlphas = 0.8,
            lineColor = rates$color[rt], showBalloon = FALSE, columnWidth = 1)
  })
  chart <- c(chart,amGraph(type = 'step', valueField = 'limit', columnWidth = 0.5, 
                           lineThickness = 3, noStepRisers = TRUE, stackable = FALSE))
  chart <- c(chart,amGraph(type = 'column', valueField = 'bullet', columnWidth = 0.3, fillAlphas = 1, clustered = FALSE, stackable = FALSE))
  pipeR::pipeline(
    amSerialChart(dataProvider = dataProvider , categoryField = 'category',
                  columnWidth = 1) ,
    addValueAxes(stackType = 'regular', gridAlpha = 0),
    setGraphs(chart)
  )
 "
})
