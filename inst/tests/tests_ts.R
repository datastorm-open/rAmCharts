
n <- 1000000
data <- data.frame(date = seq(as.POSIXct("1999-12-31 13:00:00", tz = "UTC"), by = "5 min", length.out = n),
                   value = rnorm(n, 10000, 50))

col_date = "date"
col_series = c("value")
maxPoints = 1000
tz = "UTC"
ts = c("5 min",  "10 min", "30 min", "hour", "3 hour", "12 hour", "day", "week", "month", "year")
fun_aggr = "mean"
treat_missing = FALSE
maxgap = Inf
type_aggr = "first"


t <- readRDS("tmp.RDS")
head(t$data)
tail(t$data)
t$zoom

data = t$data
zoom = t$zoom
col_date = t$col_date
col_series = unlist(t$col_series)
maxPoints = t$maxPoints
tz = t$tz
ts = t$ts
fun_aggr = t$fun_aggr
treat_missing = t$treat_missing
maxgap = t$maxgap
type_aggr = t$type_aggr

init_data <- getCurrentStockData(data, zoom = zoom, col_date = col_date, col_series = col_series, 
                                 maxPoints = maxPoints, tz = tz, ts = ts, 
                                 fun_aggr = fun_aggr, treat_missing = treat_missing, 
                                 maxgap = maxgap, type_aggr = type_aggr)
head(init_data$data)
init_data <- getCurrentStockData(data, col_date = col_date, col_series = col_series, 
                                 maxPoints = maxPoints, tz = tz, 
                                 ts =c("5 min",  "10 min", "30 min", "hour", "3 hour", "12 hour", "day", 
                                       "week", "month", "year"), 
                                 fun_aggr = fun_aggr, treat_missing = treat_missing, 
                                 maxgap = maxgap, type_aggr = type_aggr)

a <- amTimeSeries(data = init_data$data, max_points = 50000,
             col_date = col_date, col_series = col_series, 
             groupToPeriods = "WW", precision = 0)

  