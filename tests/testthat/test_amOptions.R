# context("AmOptions")
# 
# 
# testthat::test_that("amOptions", {
#   
#   .testAmOptions <- function(chart){
#     #test legend
#     amOptions(chart, legend = TRUE)
#     amOptions(chart, legend = TRUE, legendPosition = "bottom")
#     amOptions(chart, legend = TRUE, legendPosition = "bottom", legendAlign = "center")
#     
#     #Export
#     amOptions(chart, export = TRUE)
#     amOptions(chart, export = TRUE, exportFormat = c("CSV","JPG"))
#     
#     #creditsPosition
#     amOptions(chart, creditsPosition = "bottom-right")
#     
#     #theme
#     amOptions(chart, theme  = "chalk")
#     
#     #main
#     amOptions(chart, main  = "My plot")
#     amOptions(chart, main  = "My plot", mainColor = "#BBBBBB")
#     amOptions(chart, main  = "My plot", mainColor = "#BBBBBB", mainSize = 50)
#     TRUE
#   }
#   
#   
#   #radar
#   data(data_radar)
#   testthat::expect_true(.testAmOptions(amRadar(data_radar)))
# 
#   #pie
#   data(data_pie)
#   testthat::expect_true(.testAmOptions(amPie(data_pie)))
#   
#   #bar
#   data(data_bar)
#   testthat::expect_true(.testAmOptions(amBarplot(x = "country", y = "visits", data_bar)))
#   data(data_gbar)
#   testthat::expect_true(.testAmOptions(amBarplot(x = "year", y = c("income", "expenses"), data_gbar)))
#   
#   
#   #candleStick
#   data(data_candleStick1)
#   testthat::expect_true(.testAmOptions(amCandlestick(data_candleStick1)))
#   data(data_candleStick2)
#   testthat::expect_true(.testAmOptions(amCandlestick(data_candleStick2)))
#   
#   #funnel
#   data(data_funnel)
#   testthat::expect_true(.testAmOptions(amFunnel(data_funnel)))
#   
#   #waterfall
#   data(data_waterfall)
#   testthat::expect_true(.testAmOptions(amWaterfall(data_waterfall)))
#   
#   
#   #mekko
#   data(data_mekko)
#   testthat::expect_true(.testAmOptions(amMekko(x = "var1", y = "var2", data_mekko)))
#   
#   #wind
#   data(data_wind)
#   testthat::expect_true(.testAmOptions(amWind(data_wind)))
#   
#   #Boxplot
# 
#   testthat::expect_true(.testAmOptions(amBoxplot(count ~ spray, data = InsectSprays)))
#   
#   #bullet
#   testthat::expect_true(.testAmOptions( amBullet(value = 65)))
#   
#   #floating bar
#   data(data_fbar)
#   testthat::expect_true(.testAmOptions( amFloatingBar(x = "country",
#                                   y_inf = "visits_inf", y_sup = "visits_sup", data_fbar)))
#   
#   
#   #hist
#   x <- replicate(1000, {
#     if (round(runif(1))) {
#       rnorm(1)
#     } else {
#       rnorm(1, mean = 5)
#     }
#   })
#   testthat::expect_true(.testAmOptions( amHist(x)))
#   
#   
#   #plot
#   xc <- paste("cat.", 1:100)
#   y <- rnorm(length(xc))
#   testthat::expect_true(.testAmOptions(amPlot(x = xc, y = y, type = 'l')))
#   
#   #gauge
#   testthat::expect_true(.testAmOptions(amSolidGauge(x = 75, min = 0, max = 100, type = "full", width = 20, 
#                color = "#1e90ff", text = "%",
#                textSize = 50)))
#   
# })