{
data_funnel <- data.frame(description = c("Website visits", "Downloads", 
                                          "Requested price list", 
                                          "Contaced for more info",
                                          "Purchased", "Contacted for support",
                                          "Purchased additional products"), 
                          value = c(300, 123, 98, 72, 80, 15, 8),
                          stringsAsFactors = FALSE)
devtools::use_data(data_funnel, overwrite = TRUE)
rm(list = ls())
}