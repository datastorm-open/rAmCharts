{
  data_wind <- data.frame(weak = c(1, 2, 3, 4,1, 2,1, 2),
                          middle = c(2, 8, 1, 1,2, 8,1, 2),
                          strong = c(1, 1, 2, 2,1, 1 ,1, 2))
  devtools::use_data(data_wind, overwrite = TRUE)
  rm(list = ls())
}