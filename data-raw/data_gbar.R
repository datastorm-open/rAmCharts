{
  data_gbar <- data.frame(year = c("2005", "2006", "2007", "2008", "2009"),
                          day = c("01/06/2005", "02/06/2005", "03/06/2005",
                                  "04/06/2005", "05/06/2005"),
                          month = c("06/2005", "07/2005", "08/2005",
                                    "09/2005", "10/2005"),
                          income = c(23.5, 26.2, 30.1, 29.5, 24.6),
                          expenses = c(18.1, 22.8, 23.9, 25.1, 25),
                          stringsAsFactors = FALSE)
  
  devtools::use_data(data_gbar, overwrite = TRUE)
  rm(list = ls())
}


