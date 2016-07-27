{
  data_gantt <-  data.frame( category= c("Ben",
                                   "Ben",
                                   "Mike",
                                   "Mike",
                                   "Simon"),
                       begin = as.Date(c("2016-04-05",
                                         "2016-07-29",
                                         "2016-05-08",
                                         "2016-08-08",
                                         "2017-05-01")),
                       end =  as.Date(c("2016-06-05",
                                        "2016-09-29",
                                        "2016-07-08",
                                        "2017-05-08",
                                        "2017-06-01")),
                       color = c("#0000FF",
                                 "#FF0000",
                                 "#0000FF",
                                 "#FF0000",
                                 "#0000FF")
  )
  devtools::use_data(data_gantt, overwrite = TRUE)
  rm(list = ls())
}