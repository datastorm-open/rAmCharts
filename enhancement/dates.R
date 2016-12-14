##Data
data('data_candleStick2')
data_candleStick2$category <- c('2015-01-01', '2015-02-01', '2015-03-01',
                                '2015-04-01', '2015-05-01', '2015-06-01',
                                '2015-07-01', '2015-08-01', '2015-09-01',
                                '2015-10-01', '2015-11-01', '2015-12-01')
##Plot
amCandlestick(data = data_candleStick2, dataDateFormat = 'YYYY-MM-DD', minPeriod = 'MM')