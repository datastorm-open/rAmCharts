
{
  data_mekko <- data.frame(var1 = c(rep("A1", 150), rep("A2", 350), rep("A3", 500)),
                           var2 = sample(c("B1", "B2", "B3", "B4", "B5", "B6"), 
                                         1000, replace = TRUE))
devtools::use_data(data_mekko, overwrite = TRUE)
rm(list = ls())
}