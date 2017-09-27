#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rAmCharts)

ui <- fluidPage(
  amChartsOutput("serial")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  data <- reactive({
    data.frame(x = 1:10, y = round(runif(10), 2))
  })
  
  output$serial <- renderAmCharts({
    ##Plot
    data <- data()
    graph <- pipeR::pipeline(
      amXYChart(dataProvider = data, hideYScrollbar = TRUE),
      addGraph(balloonText = 'x:<b>[[x]]</b> y:<b>[[y]]</b>',
               bullet = 'circle', lineAlpha=0, xField = 'x',yField = 'y', maxBulletSize = 100),
      setChartCursor(),
      # updateOnReleaseOnly : wait for user releases mouse button.
      setChartScrollbar(updateOnReleaseOnly = TRUE)
      
      
    )
    graph@valueAxes <- list(
      list(position = 'bottom', strictMinMax = TRUE, minimum = 0, maximum = 11, includeGuidesInMinMax = TRUE,
           listeners = list("axisZoomed" =  htmlwidgets::JS("function (event) {
              var zoomed_event = event.chart.valueAxes[0].events.axisZoomed;
              event.chart.valueAxes[0].events.axisZoomed = [];
              event.chart.valueAxes[0].zoomToValues(event.chart.valueAxes[0].prevStartValue, event.chart.valueAxes[0].prevEndValue);
              event.chart.valueAxes[0].events.axisZoomed = zoomed_event;
              Shiny.onInputChange('zoom', {start : event.startValue, end : event.endValue})
              }"))))
    graph
  })
  

  shiny::observe({
    cur_zoom <- input$zoom
    if(!is.null(cur_zoom)){

      print(cur_zoom)
      # create new data
      new_data <- data.frame(x = as.numeric(cur_zoom$start):as.numeric(cur_zoom$end))
      new_data$y <- rnorm(nrow(new_data))

      if(as.numeric(cur_zoom$start) > 1){
        new_data <- rbind(data.frame(x = 1, y = NA), new_data)
      }
      
      if(as.numeric(cur_zoom$end) < 10){
        new_data <- rbind(new_data,
                          data.frame(x = 10, y = NA))
      }

      print(new_data)
      session$sendCustomMessage("amXYUpdateData",
                                list("serial", jsonlite::toJSON(new_data), TRUE))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

