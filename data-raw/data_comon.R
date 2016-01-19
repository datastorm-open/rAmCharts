# ---
# data_gdp
# ---

{
  data_gdp <- data.table(country = c('China', 'United States', 'India', 'Japan', 'Germany',
                                     'Russia', 'Brazil', 'Indonesia', 'United Kingdom', 'France'),
                         gdp = c(18.976, 18.125, 7.997, 4.843, 3.815, 3.458, 3.259, 2.840, 2.641, 2.634))
  
  devtools::use_data(data_gdp , overwrite = TRUE)
  rm(list = ls())
}

# ---
# data_AirPassengers
# ---

{
data_AirPassengers <- as.data.frame(get(x = "AirPassengers", pos = "package:datasets"))
setnames(data_AirPassengers, "x", "AirPassengers")
data_AirPassengers$AirPassengers <- as.numeric(data_AirPassengers$AirPassengers)
data_AirPassengers$Period <- paste0(rep(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), 12),
                                    "/" , rep(1949 : 1960, each = 12))
temp <- data_AirPassengers$AirPassengers 
data_AirPassengers$Bsup <- temp + 1:length(temp) / 5 + temp / 10
data_AirPassengers$Binf <- temp - 1:length(temp) / 5 - temp / 10

data_AirPassengers$AirPassengers2 <- temp / 4
data_AirPassengers$Bsup2 <- data_AirPassengers$Bsup / 4
data_AirPassengers$Binf2 <- data_AirPassengers$Binf / 4

devtools::use_data(data_AirPassengers, overwrite = TRUE)
rm(list = ls())
}
