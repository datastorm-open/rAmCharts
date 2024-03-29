% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chart_amBoxplot.R
\name{amBoxplot}
\alias{amBoxplot}
\alias{amBoxplot.default}
\alias{amBoxplot.data.frame}
\alias{amBoxplot.matrix}
\alias{amBoxplot.formula}
\title{Plotting boxplot using rAmCharts}
\usage{
amBoxplot(object, ...)

\method{amBoxplot}{default}(
  object,
  xlab = NULL,
  ylab = NULL,
  ylim = NULL,
  names = NULL,
  col = "#1e90ff",
  horiz = FALSE,
  ...
)

\method{amBoxplot}{data.frame}(
  object,
  id = NULL,
  xlab = NULL,
  ylab = NULL,
  ylim = NULL,
  col = NULL,
  horiz = FALSE,
  ...
)

\method{amBoxplot}{matrix}(
  object,
  use.cols = TRUE,
  xlab = NULL,
  ylab = NULL,
  ylim = NULL,
  col = NULL,
  horiz = FALSE,
  ...
)

\method{amBoxplot}{formula}(
  object,
  data = NULL,
  id = NULL,
  xlab = NULL,
  ylab = NULL,
  ylim = NULL,
  col = NULL,
  horiz = FALSE,
  ...
)
}
\arguments{
\item{object}{a vector, data.frame, a matrix, or a formula.}

\item{...}{see \link{amOptions} for more options.}

\item{xlab, ylab}{\code{character}, labels of the axis.}

\item{ylim}{\code{numeric}, y values range with sensible defaults.}

\item{names}{\code{character}, name on x-axis, if object is a vector.}

\item{col}{\code{character}, color(s) to be used to fill the boxplot.}

\item{horiz}{\code{logical}, TRUE to rotate chart.}

\item{id}{\code{character},  column name of id to 
identify outliers, if object is a dataframe.}

\item{use.cols}{\code{logical}, for matrix only. Set to TRUE to display boxplot
based on columns.}

\item{data}{\code{data.frame}, from which the variables in formula should be taken.}
}
\value{
An object of class \linkS4class{AmChart}.
}
\description{
amBoxplot computes a boxplot of the given data values.
Can be a vector, a data.frame, or a matrix.
}
\examples{

\dontrun{
# 'numeric' (default)
amBoxplot(rnorm(100))

# 'formula'
amBoxplot(count ~ spray, data = InsectSprays)

# 'formula', two group
data <- InsectSprays
data$group <- c("H", "F")
amBoxplot(count ~ spray + group, data = data, col = c("purple", "darkblue"))

# 'matrix'
x <- matrix(nrow = 10, ncol = 5, rnorm(50))
amBoxplot(x)

# 'data.frame'
amBoxplot(iris[, 1:4])

}
# Other examples available which can be time consuming depending on your configuration.

\dontrun{
don <- data.frame(a = 1:10, b = 1:5)
amBoxplot(don, ylim = c(0,15))
}
\dontrun{
# --- matrix
x <- matrix(nrow = 10, ncol = 5, rnorm(50))

amBoxplot(x) # on columns
colnames(x) <- LETTERS[1:5]
amBoxplot(x) # with names
amBoxplot(x, use.cols = FALSE, col = c("blue", "red"))

# Parameter for amOptions
amBoxplot(x, export = TRUE, exportFormat = "SVG")
}
\dontrun{
# --- Formula
(obj <- amBoxplot(count ~ spray, data = InsectSprays))

# Adding parameters
amBoxplot(count ~ spray, data = InsectSprays, ylim = c(0,50),
          xlab = "spray", col = c("darkblue", "gray"))

# Transpose
amBoxplot(count ~ spray, data = InsectSprays, ylim = c(0,50), xlab = "spray", horiz = FALSE)

# Using a custom colum to identify outliers
InsectSprays$id <- paste0("ID : ", 1:nrow(InsectSprays))
amBoxplot(count ~ spray, data = InsectSprays, id = "id")

# Parameter for amOptions
amBoxplot(count ~ spray, data = InsectSprays, main = "amcharts")
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
