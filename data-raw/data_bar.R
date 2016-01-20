{
data_bar <- data.frame(
  country = c('USA', 'China', 'Japan', 'Germany','UK', 'France',
              'India', 'Spain','Netherlands', 'Russia', 'South Korea','Canada'),
  visits = c(3025, 1882, 1809, 1322, 1122, 1114, 984, 711, 665, 580, 443, 441),
  color = c('#FF0F00', '#FF6600', '#FF9E01', '#FCD202', '#F8FF01', '#B0DE09', 
            '#04D215', '#0D8ECF', '#0D52D1', '#2A0CD0', '#8A0CCF', '#CD0D74'),
  stringsAsFactors = FALSE
)
devtools::use_data(data_bar, overwrite = TRUE)
rm(list = ls())
}