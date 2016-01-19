# -------------------------------------------------------------
# Data for stock charts
# -------------------------------------------------------------

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

#' @title 10 Richest Countries in the World by 2015 GDP
#' @description Value in $ trillion
#' @format Dataset of 2 columns and 10 rows
#' \describe{
#'   \item{country}{\code{character}}
#'   \item{gdp}{\code{numeric}}
#' }
#' @source \url{http://www.insidermonkey.com/blog/10-richest-countries-in-the-world-by-2015-gdp-344692/}
"data_gdp"


#' @title Air passengers for example
#' @description Based on the dataset 'AirPassengers' from the package 'datasets'
#' @format 2 column, 144 rows :
#' \describe{
#'   \item{AirPassengers}{\code{numeric}}
#'   \item{Period}{\code{character}, MM/YYYY}
#' }
"data_AirPassengers"


# -------------------------------------------------------------
# Data for candlestick charts
# -------------------------------------------------------------

# ---
# candleStick1
# ---

#' @title Random data for plotting candlestick chart examples
#' @description This dataset is used in the tutorial,
#' moreover if you notice a bug, use this dataset to give us an example.
#' @format Dataset of 5 columns and 12 rows
#' \describe{
#'   \item{category}{\code{character}, can be parsed as a date}
#'   \item{open}{\code{numeric}}
#'   \item{close}{\code{numeric}}
#'   \item{low}{\code{numeric}}
#'   \item{high}{\code{numeric}}
#' }
"data_candleStick1"

# ---
# candleStick2
# ---

#' @title Random data for plotting candlestick chart examples
#' @description This dataset is used in the tutorial,
#' moreover if you notice a bug, use this dataset to give us an example.
#' @format Dataset of 5 columns and 12 rows
#' \describe{
#'   \item{category}{\code{character}, can be parsed as a date}
#'   \item{open}{\code{numeric}}
#'   \item{close}{\code{numeric}}
#'   \item{low}{\code{numeric}}
#'   \item{high}{\code{numeric}}
#' }
"data_candleStick2"

# -------------------------------------------------------------
# Data for waterfall charts
# -------------------------------------------------------------


#' @title Random data for plotting candlestick chart examples
#' @description This dataset is used in the examples,
#' moreover it can be used as a reference if you notice a bug
#' 
#' @format Dataset of 3 columns and 15 rows
#' \describe{
#'   \item{label}{\code{character}, can be parsed as a date}
#'   \item{value}{\code{numeric}}
#'   \item{operation}{\code{numeric}}
#' }
"data_waterfall"


# -------------------------------------------------------------
# Data for wind charts
# -------------------------------------------------------------

#' @title Random data for plotting candlestick chart examples
#' @description This dataset is used in the examples,
#' moreover it can be used as a reference if you notice a bug
#' 
#' @format Dataset of 3 columns and 8 rows
#' \describe{
#'   \item{weak}{\code{numeric}}
#'   \item{middle}{\code{numeric}}
#'   \item{strong}{\code{numeric}}
#' }
"data_wind"