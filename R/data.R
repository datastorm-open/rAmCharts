#' @title Generate random data for 'stock1' example
#' @examples 
#' \dontshow{
#' # Generate and save data
#' data_stock1 <- generate_data_stock1()
#' devtools::use_data(data_stock1)
#' }
#' @noRd
generate_data_stock1 <- function()
{
  firstDate <- Sys.Date()
  chartData1 <- as.data.table(t(sapply(0:20, FUN = function(i)
  {
    date <- format(firstDate + i, "%m/%d/%Y")
    a <- round(runif(1) * (40 + i)) + 100 + i
    b <- round(runif(1) * (1000 + i)) + 500 + i * 2
    c(date = date, value = a,  volume = b)
  })))
  
  chartData2 <- as.data.table(t(sapply(0:20, FUN = function(i)
  {
    date <- format(firstDate + i, "%m/%d/%Y")
    a <- round(runif(1) * (100 + i)) + 200 + i
    b <- round(runif(1) * (1000 + i)) + 600 + i * 2
    c(date = date, value = a,  volume = b)
  })))
  
  chartData3 <- as.data.table(t(sapply(0:20, FUN = function(i)
  {
    date <- format(firstDate + i, "%m/%d/%Y")
    a <- round(runif(1) * (100 + i)) + 200 + i
    b <- round(runif(1) * (1000 + i)) + 600 + i * 2
    c(date = date, value = a,  volume = b)
  })))
  
  chartData4 <- as.data.table(t(sapply(0:20, FUN = function(i)
  {
    date <- format(firstDate + i, "%m/%d/%Y")
    a <- round(runif(1) * (100 + i)) + 200 + i
    b <- round(runif(1) * (1000 + i)) + 600 + i * 2
    c(date = date, value = a,  volume = b)
  })))
  list(chartData1 = chartData2, chartData2 = chartData2, chartData3 = chartData3, chartData4 = chartData4)
}

load("data/data_stock1.rda")
#' @title Random data for example
#' @description A list containing 4 datasets
#' @format Each datasetis a data.table with 21 rows and 4 variables:
#' \describe{
#'   \item{date}{vector of dates}
#'   \item{a}{random vector of data}
#'   \item{b}{random vector of data}
#' }
#'
"data_stock1"

data_gdp <- data.table(country = c('China', 'United States', 'India', 'Japan', 'Germany',
                                   'Russia', 'Brazil', 'Indonesia', 'United Kingdom', 'France'),
                       gdp = c(18.976, 18.125, 7.997, 4.843, 3.815, 3.458, 3.259, 2.840, 2.641, 2.634))
# devtools::use_data(data_gdp , overwrite = TRUE)

#' @title 10 Richest Countries in the World by 2015 GDP
#' @description Value in $ trillion
#' @format Dataset of 2 columns and 10 rows
#' \describe{
#'   \item{country}{\code{chracter}}
#'   \item{gdp}{\code{numeric}}
#' }
#' @source \url{http://www.insidermonkey.com/blog/10-richest-countries-in-the-world-by-2015-gdp-344692/}
"data_gdp"
