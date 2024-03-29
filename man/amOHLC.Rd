% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chart_amOHLC.R
\name{amOHLC}
\alias{amOHLC}
\title{Plotting OHLC chart}
\usage{
amOHLC(
  data,
  xlab = "",
  ylab = "",
  horiz = FALSE,
  zoom = TRUE,
  positiveColor = "#7f8da9",
  negativeColor = "#db4c3c",
  names = c("low", "open", "close", "high"),
  dataDateFormat = NULL,
  minPeriod = ifelse(!is.null(dataDateFormat), "DD", ""),
  ...
)
}
\arguments{
\item{data}{\code{data.frame}, dataframe with at least 5 columns : 
category, open (numeric), close (numeric), low (numeric),
high (numeric).}

\item{xlab}{\code{character}, label for x-axis.}

\item{ylab}{\code{character}, label for y-axis.}

\item{horiz}{\code{logical}, TRUE for an horizontal chart, FALSE for a vertical one}

\item{zoom}{\code{logical}, default set to TRUE : a cursor is added to the chart.}

\item{positiveColor}{\code{character}, color for positive values (in hexadecimal).}

\item{negativeColor}{\code{character}, color for negative values (in hexadecimal).}

\item{names}{\code{character}, names for the tooltip. Default to c("low", "open", "close", "high").}

\item{dataDateFormat}{\code{character}, default set to NULL. Even if your chart parses dates,
you can pass them as strings in your dataframe - 
all you need to do is to set data date format and the chart will parse dates to date objects.
Check this page for available formats.
Please note that two-digit years (YY) as well as literal month names (MMM)  are NOT supported in this setting.}

\item{minPeriod}{\code{character}, minPeriod Specifies the shortest period of your data.
This should be set only if dataDateFormat is not 'NULL'.
Possible period values:
fff - milliseconds, ss - seconds, mm - minutes, hh - hours, DD - days, MM - months, YYYY - years.
It's also possible to supply a number for increments, i.e. '15mm'
which will instruct the chart that your data is supplied in 15 minute increments.}

\item{...}{see \code{\link{amOptions}} for more options.}
}
\description{
amOHLC computes an OHLC chart of the given value.
}
\examples{
data("data_candleStick2")
amOHLC(data = data_candleStick2)

\dontrun{
# Other examples available which can be time consuming depending on your configuration.
if (requireNamespace("pipeR", quietly = TRUE)) {
require(pipeR)

# Change colors
amOHLC(data = data_candleStick2, positiveColor = "green", negativeColor = "red")

# Naming the axes
amOHLC(data = data_candleStick2, xlab = "categories", ylab = "values") \%>>\% setChartCursor()

# Rotate the labels for x axis
amOHLC(data = data_candleStick2, labelRotation = 90)

# Change names
amOHLC(data = data_candleStick2, names = c("min", "begin", "end", "max")) \%>>\% setChartCursor()

# Use amOptions
amOHLC(data = data_candleStick2, zoom = FALSE)
}
}

}
\references{
See online documentation \url{https://datastorm-open.github.io/introduction_ramcharts/}
and \link{amChartsAPI}
}
\seealso{
\link{amOptions}, \link{amBarplot}, \link{amBoxplot}, \link{amHist}, \link{amPie},
\link{amPlot}, \link{amTimeSeries}, \link{amStockMultiSet}, \link{amBullet}, \link{amRadar}, 
\link{amWind}, \link{amFunnel}, \link{amAngularGauge}, \link{amSolidGauge}, \link{amMekko},
\link{amCandlestick}, \link{amFloatingBar}, \link{amOHLC}, \link{amWaterfall}
}
