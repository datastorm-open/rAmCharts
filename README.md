### Introduction

This package allows the charts from the library [AmCharts][url_amcharts] using
[Hmlwidgets][url_htmlwidgets].

---

# Installation

If necessary install the *devtools* package

```{r, eval = FALSE}
if ( require(devtools) ) {
  install.packages(devtools)
} else{}
```

The package require some other functionnal packages:
```{r, eval = FALSE}
if (!require("shiny")){
  install.packages("shiny")  
}else{}

if (!require("pipeR")){
  install.packages("pipeR")
}else{}

if (!require("htmlwidgets")){
  install.packages('htmlwidgets')
}else{}

if (!require("rlist")){
  install.packages("rlist")
}else{}

if (!require("data.table")){
  install.packages("data.table")
}else{}
```
Those packages are indicated in the ***DESCRIPTION*** file below the *Depends* tag


[url_amcharts]: http://www.amcharts.com
[url_htmlwidgets]: http://www.htmlwidgets.org
