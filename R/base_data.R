# -------------------------------------------------------------
# Data for stock charts
# -------------------------------------------------------------

#' @title Random data for example
#' @description Times Series on 2015-2016, one data by hour
#' @format Each datasetis a data.table with 21 rows and 4 variables:
#' \describe{
#'   \item{date}{vector of datesTimes}
#'   \item{ts1}{random vector of data}
#'   \item{ts2}{random vector of data}
#' }
#'
"data_stock_2"

#' @title Random data for example
#' @description Times Series on 2017, by months
#' @format List of 4 datasets, 4 variables in each
#' \describe{
#'   \item{date}{vector of datesTimes}
#'   \item{value}{random vector of data}
#'   \item{volume}{random vector of data}
#'   \item{value2}{random vector of data}
#'   \item{value3}{random vector of data}
#' }
#'
"data_stock_3"

#' @title Random data for example
#' @description A list containing 4 datasets.
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
#' @source \url{https://www.insidermonkey.com/blog/10-richest-countries-in-the-world-by-2015-gdp-344692/}
"data_gdp"


#' @title Air passengers for example
#' @description Based on the dataset 'AirPassengers' from the package 'datasets'.
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
#' moreover it can be used as a reference if you notice a bug.
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

#' @title Random data for plotting wind chart examples
#' @description This dataset is used in the examples,
#' moreover it can be used as a reference if you notice a bug.
#' 
#' @format Dataset of 3 columns and 8 rows
#' \describe{
#'   \item{weak}{\code{numeric}}
#'   \item{middle}{\code{numeric}}
#'   \item{strong}{\code{numeric}}
#' }
"data_wind"


#' @title Random data for plotting radar chart examples
#' @description This dataset is used in the examples,
#' moreover it can be used as a reference if you notice a bug.
#' 
#' @format Dataset of 4 columns and 5 rows
#' \describe{
#'   \item{label}{\code{character}}
#'   \item{Product1}{\code{numeric}}
#'   \item{Product2}{\code{numeric}}
#'   \item{Product3}{\code{numeric}}
#' }
"data_radar"


#' @title Random data for plotting pie chart examples
#' @description This dataset is used in the examples,
#' moreover it can be used as a reference if you notice a bug.
#' 
#' @format Dataset of 2 columns and 5 rows
#' \describe{
#'   \item{label}{\code{character}}
#'   \item{value}{\code{numeric}}
#' }
"data_pie"


#' @title Random data for plotting bar chart examples
#' @description This dataset is used in the examples,
#' moreover it can be used as a reference if you notice a bug.
#' 
#' @format Dataset of 3 columns and 12 rows
#' \describe{
#'   \item{country}{\code{character}}
#'   \item{visits}{\code{numeric}}
#'   \item{color}{\code{character}}
#' }
"data_bar"


#' @title Random data for plotting bar chart examples
#' @description This dataset is used in the examples,
#' moreover it can be used as a reference if you notice a bug.
#' 
#' @format Dataset of 5 columns and 12 rows
#' \describe{
#'   \item{year}{\code{character}}
#'   \item{day}{\code{character}}
#'   \item{month}{\code{character}}
#'   \item{income}{\code{numeric}}
#'   \item{expenses}{\code{numeric}}
#' }
"data_gbar"



#' @title Random data for plotting funnel chart examples
#' @description This dataset is used in the examples,
#' moreover it can be used as a reference if you notice a bug.
#' 
#' @format Dataset of 2 columns and 7 rows
#' \describe{
#'   \item{description}{\code{character}}
#'   \item{value}{\code{numeric}}
#' }
"data_funnel"

#' @title Random data for plotting mekko chart examples
#' @description This dataset is used in the examples,
#' moreover it can be used as a reference if you notice a bug.
#' 
#' @format Dataset of 2 columns and 1000 rows
#' \describe{
#'   \item{var1}{\code{character}}
#'   \item{var2}{\code{numeric}}
#' }
"data_mekko"


#' @title Random data for plotting floating bar chart examples
#' @description This dataset is used in the examples,
#' moreover it can be used as a reference if you notice a bug.
#' 
#' @format Dataset of 2 columns and 1000 rows
#' \describe{
#'   \item{country}{\code{character}}
#'   \item{visits_inf}{\code{numeric}}
#'   \item{visits_sup}{\code{numeric}}
#'   \item{color}{\code{character}}
#' }
"data_fbar"

#' @title Random data for plotting gantt chart examples
#' @description This dataset is used in the examples,
#' moreover it can be used as a reference if you notice a bug.
#' 
#' @format Dataset of 5 columns and 4 rows
#' \describe{
#'   \item{category}{\code{character}}
#'   \item{begin}{\code{date}}
#'   \item{end}{\code{date}}
#'   \item{color}{\code{character}}
#' }
"data_gantt"
