{
data_pie <- data.frame(label = c("Facebook", "Twitter", "LinkedIn", "Google+", 
                                 "Pinterest"),
                       value = c(38, 25, 15, 14, 8), stringsAsFactors = FALSE)
devtools::use_data(data_pie, overwrite = TRUE)
}