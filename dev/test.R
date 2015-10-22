pipeR::pipeline(
  amSerialChart(dataDateFormat = 'YYYY',categoryField = 'year', startDuration = 0),
  setDataProvider(data.frame(year = 1990:2015, value = runif(length(1990:2015), -1, 1))),
  addValueAxes(axisAlpha = 0, position = 'left'),
  addGraph(id = 'g1', type =  'smoothedLine', valueField =  'value'),
  setChartScrollbar(graph = 'g1', oppositeAxis = FALSE),
  setCategoryAxis(minPeriod = 'YYYY', parseDates = TRUE, minorGridAlpha = 0.1, minorGridEnabled = TRUE),
  addListener('rendered', 'function(event) {alert("a");}')
)

pipeR::pipeline(
  amSerialChart(dataDateFormat = 'YYYY',categoryField = 'year', startDuration = 0),
  setDataProvider(data.frame(year = 1990:2015, value = runif(length(1990:2015), -1, 1))),
  addValueAxes(axisAlpha = 0, position = 'left'),
  addGraph(id = 'g1', type =  'smoothedLine', valueField =  'value'),
  setChartScrollbar(graph = 'g1', oppositeAxis = FALSE),
  setCategoryAxis(minPeriod = 'YYYY', parseDates = TRUE),
  setChartCursor(),
  setExport()
)

# Define the UI
ui <- bootstrapPage(
  amChartsOutput("test",type = "serial")
)


# Define the server code
server <- function(input, output) {
  output$test <- renderAmCharts({
    df <- data.frame(binf=c(1,2,3,4,5),bsup = c(3,4,5,6,7), value = c(2,3,4,5,6),heure = c("9h","10h","11h","12h","13h"))
    titre <- "essai"
    
    legend <- amLegend(equalWidths=FALSE, position="bottom", valueAlign="left", valueWidth=100
    ) %>>% addListener("hideItem" , 'function(event){
  var id = event.dataItem.id;
                       event.chart.hideGraph(event.chart.getGraphById(id + "_from"));
                       event.chart.hideGraph(event.chart.getGraphById(id + "_to"));
  }') %>>% addListener("showItem" , 'function(event){
                       var id = event.dataItem.id;
                       event.chart.showGraph(event.chart.getGraphById(id + "_from"));
                       event.chart.showGraph(event.chart.getGraphById(id + "_to"));
  }')

    
    amSerialChart(legend = legend)%>>%
      setDataProvider(df) %>>%
      setProperties(type="serial",addClassNames=TRUE,theme="light",autoMargins=TRUE,startDuration=0.5,categoryField="heure",export = list(enabled=TRUE))%>>%
      setBalloon(adjustBorderColor=FALSE,horizontalPadding=10,verticalPadding=8,color="#ffffff")%>>%
      addValueAxes(axisAlpha=0,position="left")%>>%
      setGraphs(list(
        amGraph(id = "2014_from", lineAlpha = 0, valueField = "binf", showBalloon = FALSE, visibleInLegend = FALSE),
        amGraph(id = "2014_to", lineAlpha = 0, fillAlphas = 0.2, fillToGraph = "2014_from",valueField = "bsup", 
                showBalloon = FALSE, visibleInLegend = FALSE),
        amGraph(id = "2014", valueField = "value")))%>>%
      setCategoryAxis(gridPosition="start",axisAlpha=0,tickLength=0)%>>%
      setChartCursor(cursorAlpha=1)%>>%
      addTitle(text=titre)%>>%
      plot
})
  }

# Return a Shiny app object
shinyApp(ui = ui, server = server)

test <- list(a = list(d=1, e = 3), b = 2)
rlist::list.remove(test$a, "d")

pipeR::pipeline(
  amHist(iris$Sepal.Length, freq = FALSE, breaks = 30, col = "#3c8dbc"),
  setExport()
)

pipeR::pipeline(
  amHist(iris$Sepal.Length),
  setExport()
)
