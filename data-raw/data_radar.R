{
  data_radar <- data.frame(label = c("A", "Z", "E", "R", "T"),
                           Product1 = c(1, 2, 3, 4, 2), 
                           Product2 = c(2, 8, 1, 1, 0),
                           Product3 = c(1, 1, 2, 2, 4))
  devtools::use_data(data_radar, overwrite = TRUE)
  rm(list = ls())
}