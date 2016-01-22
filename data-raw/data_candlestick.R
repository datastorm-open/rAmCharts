# ---
# candlestick 1
# ---

{
set.seed(9)
category <- c("2015-01-01", "2015-01-02", "2015-01-03", "2015-01-04",
              "2015-01-05", "2015-01-06", "2015-01-07", "2015-01-08",
              "2015-01-09", "2015-01-10", "2015-01-11", "2015-01-12", "2015-01-20")
low <- rnorm(n = length(category) - 1, mean = 130, sd = 10)
open <- low + rnorm(n = length(category) - 1, mean = 4)
close <- open + rnorm(n = length(category) - 1, mean = 0)
high <- close + rnorm(n = length(category) - 1, mean = 4)

data_candleStick1 <- data.frame(category = category,
                                open = c(open, 136.65), close = c(close, 136.4),
                                low = c(low, 134.45), high = c(high, 137.5),
                                stringsAsFactors = FALSE)
devtools::use_data(data_candleStick1, overwrite = TRUE)
rm(list = ls())
}


# ---
# candlestick 2
# ---

{
data_candleStick2 <- data.frame(category = c('2015-01-01', '2015-01-02', '2015-01-03',
                                             '2015-01-04', '2015-01-05', '2015-01-06',
                                             '2015-01-07', '2015-01-08', '2015-01-09',
                                             '2015-01-10', '2015-01-11', '2015-01-20'),
                                open = c(136.65, 135.26, 132.90, 134.94, 136.76, 131.11,
                                         123.12, 128.32, 128.29, 122.74, 117.01, 122.01),
                                close = c(136.4, 131.8, 135.2, 135.0, 134.0, 126.3, 
                                          125.0, 127.7, 124.0, 119.9, 117.0, 122.0),
                                low = c(134.15, 131.50, 128.30, 132.63, 132.00, 125.09,
                                        120.30, 126.50, 123.71, 119.65, 111.62, 119.82),
                                high = c(136.96, 135.95, 135.27, 137.24, 136.86, 133.00,
                                         127.75, 129.35, 128.30, 124.86, 118.50, 123.50), 
                                stringsAsFactors = FALSE)
devtools::use_data(data_candleStick2, overwrite = TRUE)
rm(list = ls())
}
