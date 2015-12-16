library(shiny)
library(rAmCharts)
library(pipeR)
library(shinydashboard)
library(visNetwork)

shinyApp(
  ui = 
    pageWithSidebar(
      headerPanel("rCharts: Highcharts"),
      sidebarPanel(
        selectInput("vf", "VraiFaux", c("F", "V"))),
      mainPanel(
        amChartsOutput("graph"),
        p(),
        visNetworkOutput("network")
      )
    ),
  
  server = function(input, output, session) {
    
    output$graph <- renderAmCharts({
      if (input$vf == "V") {NULL}
      else {
        df <- data.frame(
          country = c("USA","China","Japan","Germany","UK","France","India","Spain","Netherlands","Russia"),
          visits = c(3025,1882,1809,1322,1122,1114,984,711,665,580),
          color = c("#FF0F00","#FF6600","#FF9E01","#FCD202","#F8FF01","#B0DE09","#04D215","#0D8ECF","#0D52D1","#2A0CD0")
        )
        
        amSerialChart(theme="light",startDuration=2,categoryField="country", depth3D=40,angle=30) %>>%
          addGraph(balloonText="<b>[[category]]: [[value]]</b>",fillColorsField="color",
                   fillAlphas=0.85,lineAlpha=0.1,type="column",valueField="visits") %>>%
          setChartCursor(categoryBalloonEnabled=FALSE,cursorAlpha=0,zoomable=FALSE) %>>%
          setCategoryAxis(gridPosition="start",labelRotation=45,axisAlpha=0,gridAlpha=0) %>>%
          setDataProvider(df) %>>%
          setExport()
        
      }
    })
    
    output$network <- renderVisNetwork({
      if (input$vf == "V") {NULL}
      else {
        # minimal example
        nodes <- data.frame(id = 1:3, label = 1:3)
        edges <- data.frame(from = c(1,2), to = c(1,3))
        
        visNetwork(nodes, edges) %>%
          visInteraction(hover = TRUE)
      }
    })
  }
)
