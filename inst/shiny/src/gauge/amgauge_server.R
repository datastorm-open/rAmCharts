require(pipeR)
#basic example


output$amangular <- rAmCharts::renderAmCharts({
  ##Plot
  amAngularGauge(x = 25)
})



output$code_amangular <- renderText({
  "
  ##Plot
  amAngularGauge(x = 25)
  "
})

output$amangular2 <- rAmCharts::renderAmCharts({
  ##Data
  bands = data.frame(start = c(0, 40, 60), end = c(40, 60, 100), 
                     color = c('#00CC00', '#ffac29', '#ea3838'),
                     stringsAsFactors = FALSE)
  ##Plot
  amAngularGauge(x = 25, bands = bands)
})


output$code_amangular2 <- renderText({
  "
  ##Data
  bands = data.frame(start = c(0, 40, 60), end = c(40, 60, 100), 
                     color = c('#00CC00', '#ffac29', '#ea3838'),
                     stringsAsFactors = FALSE)
  ##Plot
  amAngularGauge(x = 25, bands = bands)
  "
})




output$amangular3 <- rAmCharts::renderAmCharts({
  ##Data
bands <- data.frame(start = c(0, 40, 60), end = c(40, 60, 100), 
                   color = c('#00CC00', '#ffac29', '#ea3838'),
                   stringsAsFactors = FALSE)
bands2 <- data.frame(start = c(100, 130, 170), end = c(130, 170, 200), 
                    color = c('#00CC00', '#ffac29', '#ea3838'),
                    stringsAsFactors = FALSE)
  ##Plot
amAngularGauge(x = 25, start = 0, end = 100, bands = bands,
               secondAxe = TRUE, start2 = 100, end2 = 200, bands2 = bands2, text = 'km/h')
})


output$code_amangular3 <- renderText({
  "
  ##Data
  bands <- data.frame(start = c(0, 40, 60), end = c(40, 60, 100), 
                      color = c('#00CC00', '#ffac29', '#ea3838'),
                      stringsAsFactors = FALSE)
  bands2 <- data.frame(start = c(100, 130, 170), end = c(130, 170, 200), 
                       color = c('#00CC00', '#ffac29', '#ea3838'),
                       stringsAsFactors = FALSE)
  ##Plot
  amAngularGauge(x = 25, start = 0, end = 100, bands = bands,
                 secondAxe = TRUE, start2 = 100, end2 = 200, bands2 = bands2, text = 'km/h')
  "
})

output$amangular4 <- rAmCharts::renderAmCharts({
  amSolidGauge(x = 25)
})

output$code_amangular4 <- renderText({
  "
  amSolidGauge(x = 25)
  "
})