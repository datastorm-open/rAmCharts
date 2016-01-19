#' @title Generate random data for 'stock1' example
#' @examples 
#' \dontshow{
#' # Generate and save data
#' data_stock1 <- .generate_data_stock1()
#' devtools::use_data(data_stock1)
#' }
#' @import data.table
#' @noRd
.generate_data_stock1 <- function()
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
  list(chartData1 = chartData2, chartData2 = chartData2, chartData3 = chartData3, chartData4 = chartData4)
}