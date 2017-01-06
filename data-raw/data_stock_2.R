{
  dateseq <- seq(as.POSIXct("2015-01-01"), as.POSIXct("2015-03-01"), by = "hour")
  dateseq <- dateseq[-length(dateseq)]
  ts1 <- cos(seq(-pi, pi, length.out = 24))*500
  ts2 <- sin(seq(-pi, pi, length.out = 24))*500
  
  ts1 <- rep_len(ts1, length(dateseq))
  ts2 <- rep_len(ts2, length(dateseq))
  
  ts1 <- ts1 + 1:length(dateseq)/50
  ts2 <- ts2 + 1:length(dateseq)/50
  
  ts1 <- ts1 + runif(length(dateseq)) * 200
  ts2 <- ts2 + runif(length(dateseq)) * 200
  
  ts1 <- round(ts1, 0)
  ts2 <- round(ts2, 0)
  
  data_stock_2 <- data.frame(date = dateseq, ts1 = ts1, ts2 = ts2)
  devtools::use_data(data_stock_2, overwrite = TRUE)
}