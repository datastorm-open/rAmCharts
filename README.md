[![Rdoc](http://www.rdocumentation.org/badges/version/rAmCharts)](http://www.rdocumentation.org/packages/rAmCharts)
[![Build Status](https://travis-ci.org/datastorm-open/rAmCharts.svg?branch=master)](https://travis-ci.org/datastorm-open/rAmCharts)
[![Rdoc](http://www.rdocumentation.org/api/badges/Direct/rAmCharts)](http://www.rdocumentation.org/packages/rAmCharts)

## Introduction

This package allows to draw interactive charts from the *JavaScript* library [AmCharts][url_amcharts] using [Hmtlwidgets][url_htmlwidgets].

Currently available chart types: funnel, gantt, gauge, pie, radar, serial, stock, xy.

Please refer to this page http://datastorm-open.github.io/introduction_ramcharts/, you will find several examples and a quick tutorial.

Moreover, full [AmCharts] API is available in the package using ``amChartsAPI()``.

---

## Installation

The version 2.1.7 is available on CRAN:

```{r, eval=FALSE}
install.packages("rAmCharts")
```

To install the "dev version", run the following code lines:

```{r, eval = FALSE}
if (!require(devtools)) {
  install.packages("devtools")
} else {}

devtools::install_github("datastorm-open/rAmCharts")
```

**NB**:

* Version 1.1.2 is based on [AmCharts][url_amcharts] v3.17.2
* Version 2.0.0 is based on [AmCharts][url_amcharts] v3.18.2
* Version 2.0.2 is based on [AmCharts][url_amcharts] v3.20.3
* Version 2.1.0 is based on [AmCharts][url_amcharts] v3.20.10
* Version <= 2.1.7 is based on [AmCharts][url_amcharts] v3.20.18
* Version >= 2.1.8 is based on [AmCharts][url_amcharts] v3.21.13

### Known issues

* Problem in Shiny with Firefox (works with Chrome or Safari), the function `renderAmcharts({NULL})` does not clear the chart, use conditionalPanel instead.
* Use in R Markown needs either an url path "http://www.amcharts.com/lib/3" to find images or a local path e.g. `system.file("htmlwidgets/lib", package = "rAmCharts")`. HTML reports also need a call to the method `plot`.

```{r, eval = FALSE}
library(rAmCharts)
data(iris)

amHist(iris$Sepal.Length, freq = FALSE, breaks = 30, col = "gray",
       path = "http://www.amcharts.com/lib/3")
# path = system.file("htmlwidgets/lib", package = "rAmCharts"))

```

* Usual R colors work (for instance, light or dark prefixes), however the 'export' feature needs valid CSS colors. To be sure, use hexadecimal format if you want to use the 'export' feature.

[url_amcharts]: http://www.amcharts.com
[url_htmlwidgets]: http://www.htmlwidgets.org
[path_histogram]: ./img/histogram.png
[path_boxplot]: ./img/boxplot.png
