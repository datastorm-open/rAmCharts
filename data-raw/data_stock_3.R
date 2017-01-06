{
  firstDate <- Sys.Date()
  chartData1 <- as.data.table(t(sapply(0:20, FUN = function(i)
  {
    date <- format(firstDate + i, "%m/%d/%Y")
    a <- round(stats::runif(1) * (40 + i)) + 100 + i
    b <- round(stats::runif(1) * (1000 + i)) + 500 + i * 2
    c(date = date, value = a,  volume = b)
  })))
  
  chartData2 <- as.data.table(t(sapply(0:20, FUN = function(i)
  {
    date <- format(firstDate + i, "%m/%d/%Y")
    a <- round(stats::runif(1) * (100 + i)) + 200 + i
    b <- round(stats::runif(1) * (1000 + i)) + 600 + i * 2
    c(date = date, value = a,  volume = b)
  })))
  
  chartData3 <- as.data.table(t(sapply(0:20, FUN = function(i)
  {
    date <- format(firstDate + i, "%m/%d/%Y")
    a <- round(stats::runif(1) * (100 + i)) + 200 + i
    b <- round(stats::runif(1) * (1000 + i)) + 600 + i * 2
    c(date = date, value = a,  volume = b)
  })))
  
  chartData4 <- as.data.table(t(sapply(0:20, FUN = function(i)
  {
    date <- format(firstDate + i, "%m/%d/%Y")
    a <- round(stats::runif(1) * (100 + i)) + 200 + i
    b <- round(stats::runif(1) * (1000 + i)) + 600 + i * 2
    c(date = date, value = a,  volume = b)
  })))

  chartData1$value2 <- as.numeric(chartData1$value) + 10
  chartData2$value2 <- as.numeric(chartData2$value) + 30
  chartData3$value2 <- as.numeric(chartData3$value) + 60
  chartData4$value2 <- as.numeric(chartData4$value) + 80

  chartData1$value3 <- as.numeric(chartData1$value) - 10
  chartData2$value3 <- as.numeric(chartData2$value) - 30
  chartData3$value3 <- as.numeric(chartData3$value) - 25
  chartData4$value3 <- as.numeric(chartData4$value) - 55
  
  data_stock_3 <- list(chartData1 = chartData1, chartData2 = chartData2, chartData3 = chartData3, chartData4 = chartData4)
  devtools::use_data(data_stock_3, overwrite = TRUE)
  rm(list = ls())
}