require(pipeR)
#basic example
amAngularGauge(x = 25)

#add colored bands
bands = data.frame(start = c(0, 40, 60), end = c(40, 60, 100), 
                   color = c("#00CC00", "#ffac29", "#ea3838"),
                   stringsAsFactors = FALSE)
amAngularGauge(x = 25, bands = bands)
   
#add some text to the displayed value
amAngularGauge(x = 25, text = "km/h")

#a gauge with two axis
bands2 = data.frame(start = c(100, 130, 170), end = c(130, 170, 200), 
                   color = c("#00CC00", "#ffac29", "#ea3838"),
                   stringsAsFactors = FALSE)
amAngularGauge(x = 25, start = 0, end = 100, bands = bands,
               secondAxe = TRUE, start2 = 100, end2 = 200, bands2 = bands2)
