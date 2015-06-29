#' @include AmChart.R StockPanel.R
NULL

#' @title AmStockChart
#' 
#' @slot \code{categoryAxesSettings}: Object of class \code{"list"}.
#' List of a \code{\linkS4class{CategoryAxes}} properties.
#' CategoryAxesSettings settings set's settings common for all CategoryAxes of StockPanels.
#' If you change a property after the chart is initialized,
#' you should call stockChart.validateNow() method in order for it to work.
#' If there is no default value specified, default value of CategoryAxis class will be used.
#' you should get this axis from the chart and set properties to this object.
#' 
#' @slot \code{ChartCursorSettings}: Object of class \code{"list"}.
#' List of a \code{\linkS4class{ChartCursor}} class properties.
#' ChartCursorSettings settings set's settings for chart cursor.
#' If you change a property after the chart is initialized,
#' you should call stockChart.validateNow() method in order for it to work.
#' If there is no default value specified, default value of ChartCursor class will be used.
#' 
#' @slot \code{ChartScrollbarSettings}: Object of class \code{"list"}.
#' List of a \code{\linkS4class{ChartScrollbar}} class properties.
#' ChartScrollbarSettings settings set's settings for chart scrollbar.
#' If you change a property after the chart is initialized,
#' you should call stockChart.validateNow() method in order for it to work.
#' If there is no default value specified, default value of ChartScrollbar class will be used.
#' 
#' @slot \code{comparedDataSets}: Object of class \code{"list"}. Properties of data sets selected for comparing.
#' 
#' @slot \code{dataSets}: Object of class \code{"list"}.
#' Each element must a list of DataSet properties. 
#' 
#' @slot \code{dataSetSelector}: Object of class \code{"list"}. DataSetSelector properties.
#' You can add it if you have more than one data set and want users to be able to select/compare them.
#' 
#' @slot \code{legendSettings}: Object of class \code{"list"}. Legend settings.
#' 
#' @slot \code{mainDataSet}: Object of class \code{"list"}. Data set selected as main.
#' 
#' @slot \code{panels}: Object of class \code{"list"}.
#' Each element must be a list stockPanel properties.
#' 
#' @slot \code{panelsSettings}: Object of class \code{"list"}. Settings for stock panels.
#' 
#' @slot \code{periodSelector}: Object of class \code{"list"}.
#' Period selector object.
#' You can add it if you want user's to be able to enter
#' date ranges or zoom chart with predefined period buttons.
#' 
#' @slot \code{stockEventsSettings}: Object of class \code{"list"}. Settings for stock events.
#' 
#' @slot \code{valueAxesSettings}: Object of class \code{"list"}. Settings for value axes.
#' 
#' @author Dataknowledge
#' 
#' @seealso \code{\url{http://docs.amcharts.com/3/javascriptstockchart/AmStockChart}}
#' 
#' @export
setClass("AmStockChart", contains = "AmChart",
         representation = representation(
           categoryAxesSettings = "list",
           chartCursorSettings = "list",
           chartScrollbarSettings = "list",
           comparedDataSets = "list",
           dataSets = "list",
           dataSetSelector = "list",
           legendSettings = "list",
           mainDataSet = "list",
           panels = "list",
           panelsSettings = "list",
           periodSelector = "list",
           scrollbarChart = "list",
           stockEventsSettings = "list",
           valueAxesSettings = "list"
         ),
         validity = function(object)
         {
           if( object@type != "stock" ){
             stop( "[AmStockChart]: you cannot change the type when creating AmStockChart")
           }
         }
)

#' @title Initialize an AmStockChart
#' @description Method for initializing any S4 class provided by the package.
#' @examples
#' new("AmStockChart", theme = "dark")
#' @family AmChart methods
#' @seealso \code{\linkS4class{AmStockChart}} S4 class
#' @export
setMethod(f = "initialize", signature = "AmStockChart",
          definition = function( .Object,
                                 categoryAxesSettings,
                                 chartCursorSettings,
                                 chartScrollbarSettings,
                                 comparedDataSets,
                                 dataSets,
                                 dataSetSelector,
                                 legendSettings,
                                 mainDataSet,
                                 panels,
                                 panelsSettings,
                                 periodSelector,
                                 stockEventsSettings,
                                 theme,
                                 valueAxesSettings, ...)
          {
            .Object@type = "stock"
            if( !missing(categoryAxesSettings) ){
              .Object <- setCategoryAxesSettings( .Object, categoryAxesSettings)
            }else{}
            if( !missing(chartCursorSettings) ){
              .Object <- setChartCursorSettings( .Object, chartCursorSettings)
            }else{}
            if( !missing(chartScrollbarSettings) ){
              .Object <- setChartScrollbarSettings( .Object, chartScrollbarSettings)
            }else{}
            if( !missing(comparedDataSets) ){
              .Object <- setComparedDataSets( .Object, comparedDataSets)
            }else{}
            if( !missing(dataSets) ){
              .Object <- setDataSets(.Object, dataSets)
            }else{}
            if( !missing(dataSetSelector) ){
              .Object <- setDataSetSelector(.Object, dataSetSelector)
            }else{}
            if( !missing(legendSettings) ){
              .Object <- setLegendSettings( .Object, legendSettings )
            }else{}
            if( !missing(mainDataSet) ){
              .Object <- setMainDataSet( .Object, mainDataSet )
            }else{}
            if( !missing(panels) ){
              .Object <- setPanels( .Object, panels )
            }else{}
            if( !missing(panelsSettings) ){
              .Object <- setPanelsSettings( .Object, panelsSettings )
            }else{}
            if( !missing(periodSelector) ){
              .Object <- setPeriodSelector( .Object, periodSelector )
            }else{}
            if( !missing(stockEventsSettings) ){
              .Object <- setStockEventsSettings( .Object, stockEventsSettings )
            }else{}
            if( !missing(theme) ){
              .Object@theme <- theme
            }else{}
            if( !missing(valueAxesSettings) ){
              .Object <- setValueAxesSettings( .Object, valueAxesSettings )
            }else{}
            .Object <- setProperties(.Object, ...)
            validObject(.Object)
            return(.Object)
          }
)

#' @title amStockChart is a shortcut constructor 
#' for instantiating AmChart of type \code{stock}
#' @examples
#' amStockChart()
#' @rdname amStockChart
#' @export
amStockChart <- function( categoryAxesSettings,
                          chartCursorSettings,
                          chartScrollbarSettings,
                          comparedDataSets,
                          dataSets,
                          dataSetSelector,
                          legendSettings,
                          mainDataSet,
                          panels,
                          panelsSettings,
                          periodSelector,
                          stockEventsSettings,
                          theme,
                          valueAxesSettings, ...)
{
  .Object = new("AmStockChart")
  if( !missing(categoryAxesSettings) ){
    .Object <- setCategoryAxesSettings( .Object, categoryAxesSettings)
  }else{}
  if( !missing(chartCursorSettings) ){
    .Object <- setChartCursorSettings( .Object, chartCursorSettings)
  }else{}
  if( !missing(chartScrollbarSettings) ){
    .Object <- setChartScrollbarSettings( .Object, chartScrollbarSettings)
  }else{}
  if( !missing(comparedDataSets) ){
    .Object <- setComparedDataSets( .Object, comparedDataSets)
  }else{}
  if( !missing(dataSets) ){
    .Object <- setDataSets(.Object, dataSets)
  }else{}
  if( !missing(dataSetSelector) ){
    .Object <- setDataSetSelector(.Object, dataSetSelector)
  }else{}
  if( !missing(legendSettings) ){
    .Object <- setLegendSettings( .Object, legendSettings )
  }else{}
  if( !missing(mainDataSet) ){
    .Object <- setMainDataSet( .Object, mainDataSet )
  }else{}
  if( !missing(panels) ){
    .Object <- setPanels( .Object, panels )
  }else{}
  if( !missing(panelsSettings) ){
    .Object <- setPanelsSettings( .Object, panelsSettings )
  }else{}
  if( !missing(periodSelector) ){
    .Object <- setPeriodSelector( .Object, periodSelector )
  }else{}
  if( !missing(stockEventsSettings) ){
    .Object <- setStockEventsSettings( .Object, stockEventsSettings )
  }else{}
  if( !missing(theme) ){
    .Object@theme <- theme
  }else{}
  if( !missing(valueAxesSettings) ){
    .Object <- setValueAxesSettings( .Object, valueAxesSettings )
  }else{}
  .Object <- setProperties(.Object, ...)
  validObject(.Object)
  return(.Object)
}

# > @categoryAxesSettings: setters ####

#' @exportMethod setCategoryAxesSettings
setGeneric( name = "setCategoryAxesSettings",
            def = function(amChart, categoryAxesSettings = NULL , ...) {standardGeneric("setCategoryAxesSettings")} )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{categoryAxesSettings}: Object of class \code{\linkS4class{CategoryAxis}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' # Setter for categoryAxesSettings
#' amStockChart() %>>% setCategoryAxesSettings(gridPosition = "start")
#' @family AmStockChart setters
#' @family AmStockChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{CategoryAxesSettings}} S4 class
#' @name setCategoryAxesSettings
#' @rdname setCategoryAxesSettings
#' @export
setMethod( f = "setCategoryAxesSettings", signature = c("AmStockChart"),
           definition = function(amChart, categoryAxesSettings = NULL, ...)
           {
             if( is.null(categoryAxesSettings) ){
               categoryAxesSettings <- categoryAxis(...)
             }else{}
             amChart@categoryAxesSettings <- listProperties(categoryAxesSettings)
             validObject(amChart)
             return(amChart)
           }
)

# > @chartCursorSettings : setters ####

#' @exportMethod setChartCursorSettings
setGeneric(name = "setChartCursorSettings",
           def = function(.Object, chartCursorSettings = NULL, ...){ standardGeneric("setChartCursorSettings") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{chartCursorSettings}: Object of class \code{\linkS4class{ChartCursor}}.
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @examples
#' amStockChart() %>>% setChartCursorSettings( oneBallOnly = TRUE )
#' @family AmStockChart setters
#' @family AmStockChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{ChartCursorSettings}} S4 class
#' @name setChartCursorSettings
#' @rdname setChartCursorSettings
#' @export
setMethod( f = "setChartCursorSettings", signature = c("AmStockChart"),
           definition = function(.Object, chartCursorSettings = NULL, ...)
           {
             if( is.null(chartCursorSettings) ){
               chartCursorSettings <- chartCursor(...)
             }else {}
             .Object@chartCursorSettings <- listProperties(chartCursorSettings)
             validObject(.Object)
             return(.Object)
           }
)

# > @chartScrollbarSettings : setters ####

#' @exportMethod setChartScrollbarSettings
setGeneric(name = "setChartScrollbarSettings",
           def = function(.Object, chartScrollbarSettings = NULL, ...){ standardGeneric("setChartScrollbarSettings") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmChart}}.
#' @param \code{chartScrollbarSettings}: Object of class \code{\linkS4class{ChartScrollbar}}.
#' @examples
#' amStockChart() %>>% setChartScrollbarSettings()
#' @return The updated object of class \code{\linkS4class{AmChart}}.
#' @family AmStockChart setters
#' @family AmStockChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @seealso \code{\linkS4class{ChartScrollbar}} S4 class
#' @name setChartScrollbarSettings
#' @rdname setChartScrollbarSettings
#' @export
setMethod( f = "setChartScrollbarSettings", signature = c("AmStockChart"),
           definition = function(.Object, chartScrollbarSettings = NULL, ...)
           {
             if ( is.null(chartScrollbarSettings) ){
               chartScrollbarSettings <- chartScrollbar( ... )
             }else{}
             .Object@chartScrollbarSettings <- listProperties(chartScrollbarSettings)
             validObject(.Object)
             return(.Object)
           }
)

# > @comparedDataSets : setters ####

#' @exportMethod setComparedDataSets
setGeneric(name = "setComparedDataSets",
           def = function(.Object, comparedDataSets){ standardGeneric("setComparedDataSets") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmStockChart}}.
#' @param \code{comparedDataSets}: Object of class \code{\linkS4class{list}}.
#' Each element of this list must be a dataSet
#' @examples
#' amStockChart() %>>% setComparedDataSets(list(dataSet(compared = TRUE), dataSet(compared = TRUE)))
#' @return The updated object of class \code{\linkS4class{AmStockChart}}.
#' @family AmStockChart setters
#' @family AmStockChart methods
#' @seealso \code{\linkS4class{AmStockChart}} S4 class
#' @seealso \code{\linkS4class{DataSet}} S4 class
#' @name setComparedDataSets
#' @rdname setComparedDataSets
#' @export
setMethod( f = "setComparedDataSets", signature = c("AmStockChart"),
           definition = function(.Object, comparedDataSets)
           {
             rightClassElements <- prod(sapply(comparedDataSets, function(element) {is(element, "DataSet")}))
             if( !rightClassElements ){
               stop("[setComparedDataSets]: each element of comparedDataSets must be of class DataSet")
             }else{}
             .Object@comparedDataSets <- lapply(comparedDataSets, listProperties)
             validObject(.Object)
             return(.Object)
           }
)

#' @exportMethod addComparedDataSet
setGeneric(name = "addComparedDataSet",
           def = function(.Object, dataSet = NULL, ...){ standardGeneric("addComparedDataSet") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmStockChart}}.
#' @param \code{dataSet}: (optionnal) Object of class \code{\linkS4class{DataSet}}.
#' Each element of this list must be a dataSet
#' @param \code{...}: Properties of the \code{\linkS4class{DataSet}} to add.
#' @examples
#' amStockChart() %>>% addComparedDataSet(compared = TRUE)
#' @return The updated object of class \code{\linkS4class{AmStockChart}}.
#' @family AmStockChart setters
#' @family AmStockChart methods
#' @seealso \code{\linkS4class{AmStockChart}} S4 class
#' @seealso \code{\linkS4class{DataSet}} S4 class
#' @name addComparedDataSet
#' @rdname addComparedDataSet
#' @export
setMethod( f = "addComparedDataSet", signature = c("AmStockChart"),
           definition = function(.Object, dataSet = NULL, ...)
           {
             if( is.null(dataSet) && !missing(...) ){
               dataSet <- dataSet(...)
             }else{}
             .Object@comparedDataSets <- rlist::list.append(.Object@comparedDataSets,
                                                            listProperties(dataSet))
             validObject(.Object)
             return(.Object)
           }
)

# > @dataSets : setters ####

#' @exportMethod setDataSets
setGeneric(name = "setDataSets",
           def = function(.Object, dataSets){ standardGeneric("setDataSets") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmStockChart}}.
#' @param \code{dataSets}: Object of class \code{\linkS4class{list}}.
#' Each element of this list must be a dataSet
#' @examples
#' amStockChart() %>>% setDataSets(list(dataSet(compared = FALSE), dataSet(compared = FALSE)))
#' @return The updated object of class \code{\linkS4class{AmStockChart}}.
#' @family AmStockChart setters
#' @family AmStockChart methods
#' @seealso \code{\linkS4class{AmStockChart}} S4 class
#' @seealso \code{\linkS4class{DataSet}} S4 class
#' @name setDataSets
#' @rdname setDataSets
#' @export
setMethod( f = "setDataSets", signature = c("AmStockChart"),
           definition = function(.Object, dataSets)
           {
             rightClassElements <- prod(sapply(dataSets, function(element) {is(element, "DataSet")}))
             if( !rightClassElements ){
               stop("[setDataSets]: each element of dataSets must be of class DataSet")
             }else{}
             .Object@dataSets <- lapply(dataSets, listProperties)
             validObject(.Object)
             return(.Object)
           }
)

#' @exportMethod addDataSet
setGeneric(name = "addDataSet",
           def = function(.Object, dataSet = NULL, ...){ standardGeneric("addDataSet") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmStockChart}}.
#' @param \code{dataSet}: (optionnal) Object of class \code{\linkS4class{DataSet}}.
#' Each element of this list must be a dataSet
#' @param \code{...}: Properties of the \code{\linkS4class{DataSet}} to add.
#' @examples
#' amStockChart() %>>% addDataSet(compared = FALSE) %>>% addDataSet(dataSet())
#' @return The updated object of class \code{\linkS4class{AmStockChart}}.
#' @family AmStockChart setters
#' @family AmStockChart methods
#' @seealso \code{\linkS4class{AmStockChart}} S4 class
#' @seealso \code{\linkS4class{DataSet}} S4 class
#' @name addDataSet
#' @rdname addDataSet
#' @export
setMethod( f = "addDataSet", signature = c("AmStockChart"),
           definition = function(.Object, dataSet = NULL, ...)
           {
             if( is.null(dataSet) && !missing(...) ){
               dataSet <- dataSet(...)
             }else{}
             .Object@dataSets <- rlist::list.append(.Object@dataSets,
                                                            listProperties(dataSet))
             validObject(.Object)
             return(.Object)
           }
)

# > @dataSetSelector : setters ####

#' @exportMethod setDataSetSelector
setGeneric(name = "setDataSetSelector",
           def = function(.Object, ...){ standardGeneric("setDataSetSelector") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmStockChart}}.
#' @param \code{...}: Property of datSetSelector
#' @examples
#' amStockChart() %>>% setDataSetSelector(width = 180)
#' @return The updated object of class \code{\linkS4class{AmStockChart}}.
#' @family AmStockChart setters
#' @family AmStockChart methods
#' @seealso \code{\linkS4class{AmStockChart}} S4 class
#' @name setDataSetSelector
#' @rdname setDataSetSelector
#' @export
setMethod( f = "setDataSetSelector", signature = c("AmStockChart"),
           definition = function(.Object, ...)
           {
             .Object@dataSetSelector <- list(...)
             validObject(.Object)
             return(.Object)
           }
)

# > @legendSettings : setters ####

#' @exportMethod setLegendSettings
setGeneric(name = "setLegendSettings", def = function(.Object, ...){ standardGeneric("setLegendSettings") } )
#' @title Setter for LegendSettings
#' @details Use this methode in case of an AmStockChart.
#' @param \code{.Object}: Object of class \code{\linkS4class{AmStockChart}}.
#' @param \code{...}: Properties of \code{LegendSettings}.
#' @return The updated object of class \code{\linkS4class{AmStockChart}}.
#' @examples
#' # Without chaining
#' setLegendSettings(amStockChart(), equalWidths = TRUE)
#' @family AmStockChart setters
#' @family AmStockChart methods
#' @seealso \code{\linkS4class{AmChart}} S4 class
#' @name setLegendSettings
#' @rdname setLegendSettings
#' @export
setMethod( f = "setLegendSettings", signature = c("AmStockChart"),
           definition = function(.Object, ...)
           {
             .Object@legendSettings <- list(...)
             validObject(.Object)
             return(.Object)
           }
)

# > @mainDataSet: setter ###
#' @exportMethod setMainDataSet
setGeneric(name = "setMainDataSet",
           def = function(.Object, dataSet = NULL, ...){ standardGeneric("setMainDataSet") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmStockChart}}.
#' @param \code{dataSet}: (optionnal) Object of class \code{\linkS4class{DataSet}}.
#' Data set selected as main.
#' @param \code{...}: Properties of the \code{\linkS4class{DataSet}} to add.
#' @examples
#' amStockChart() %>>% setMainDataSet(showInCompare = TRUE)
#' @return The updated object of class \code{\linkS4class{AmStockChart}}.
#' @family AmStockChart setters
#' @family AmStockChart methods
#' @seealso \code{\linkS4class{AmStockChart}} S4 class
#' @seealso \code{\linkS4class{DataSet}} S4 class
#' @name setMainDataSet
#' @rdname setMainDataSet
#' @export
setMethod( f = "setMainDataSet", signature = c("AmStockChart"),
           definition = function(.Object, dataSet = NULL, ...)
           {
             if( is.null(dataSet) && !missing(...) ){
               dataSet <- dataSet(...)
             }else{}
             .Object@mainDataSet <- listProperties(dataSet)
             validObject(.Object)
             return(.Object)
           }
)

# > @panels : setters ####

#' @exportMethod setPanels
setGeneric(name = "setPanels",
           def = function(.Object, panels){ standardGeneric("setPanels") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmStockChart}}.
#' @param \code{panels}: Object of class \code{\linkS4class{list}}.
#' Each element of this list must be a stockPanel
#' @examples
#' amStockChart() %>>% setPanels(list(stockPanel(compared = TRUE), stockPanel(compared = TRUE)))
#' @return The updated object of class \code{\linkS4class{AmStockChart}}.
#' @family AmStockChart setters
#' @family AmStockChart methods
#' @seealso \code{\linkS4class{AmStockChart}} S4 class
#' @seealso \code{\linkS4class{StockPanel}} S4 class
#' @name setPanels
#' @rdname setPanels
#' @export
setMethod( f = "setPanels", signature = c("AmStockChart", "list"),
           definition = function(.Object, panels)
           {
             rightClassElements <- prod(sapply(panels, function(element) {is(element, "StockPanel")}))
             if( !rightClassElements ){
               stop("[setPanels]: each element of panels must be of class Panel")
             }else{}
             .Object@panels <- lapply(panels, listProperties)
             validObject(.Object)
             return(.Object)
           }
)

#' @exportMethod addPanel
setGeneric(name = "addPanel",
           def = function(.Object, panel = NULL, ...){ standardGeneric("addPanel") } )
#' @title Setter
#' @param \code{.Object}: Object of class \code{\linkS4class{AmStockChart}}.
#' @param \code{panel}: (optionnal) Object of class \code{\linkS4class{StockPanel}}.
#' Each element of this list must be a dataSet
#' @param \code{...}: Properties of the \code{\linkS4class{StockPanel}} to add.
#' @examples
#' amStockChart() %>>% addPanel(allowTurningOff = TRUE)
#' @return The updated object of class \code{\linkS4class{AmStockChart}}.
#' @family AmStockChart setters
#' @family AmStockChart methods
#' @seealso \code{\linkS4class{AmStockChart}} S4 class
#' @seealso \code{\linkS4class{StockPanel}} S4 class
#' @name addPanel
#' @rdname addPanel
#' @export
setMethod( f = "addPanel", signature = c("AmStockChart"),
           definition = function(.Object, panel = NULL, ...)
           {
             if( is.null(panel) && !missing(...) ){
               panel <- stockPanel(...)
             }else{}
             .Object@panels <- rlist::list.append( .Object@panels, listProperties(panel) )
             validObject(.Object)
             return(.Object)
           }
)

# > @panelSettings : setters ####

#' @exportMethod setPanelsSettings
setGeneric(name = "setPanelsSettings", def = function(.Object, ...){ standardGeneric("setPanelsSettings") } )
#' @title Setter for PanelsSettings
#' @details Use this methode in case of an AmStockChart.
#' @param \code{.Object}: Object of class \code{\linkS4class{AmStockChart}}.
#' @param \code{...}: Properties of \code{PanelsSettings}.
#' @return The updated object of class \code{\linkS4class{AmStockChart}}.
#' @examples
#' amStockChart() %>>% setPanelsSettings(backgroundAlpha = 0)
#' @family AmStockChart setters
#' @family AmStockChart methods
#' @seealso \code{\linkS4class{AmStockChart}} S4 class
#' @name setPanelsSettings
#' @rdname setPanelsSettings
#' @export
setMethod( f = "setPanelsSettings", signature = c("AmStockChart"),
           definition = function(.Object, ...)
           {
             .Object@panelsSettings <- list(...)
             validObject(.Object)
             return(.Object)
           }
)

# > @setPeriodSelector : setters ####

#' @exportMethod setPeriodSelector
setGeneric(name = "setPeriodSelector", def = function(.Object, periodSelector = NULL, ...){ standardGeneric("setPeriodSelector") } )
#' @title Setter for PeriodSelector
#' @details Use this methode in case of an AmStockChart.
#' @param \code{.Object}: Object of class \code{\linkS4class{AmStockChart}}.
#' @param \code{periodSelector}: Object of class \code{\linkS4class{PeriodSelector}}.
#' @param \code{...}: Properties of the class \code{\linkS4class{PeriodSelector}}.
#' @return The updated object of class \code{\linkS4class{AmStockChart}}.
#' @examples
#' amStockChart() %>>% setPeriodSelector(dateFormat = "DD-MM-YYYY")
#' @family AmStockChart setters
#' @family AmStockChart methods
#' @seealso \code{\linkS4class{AmStockChart}} S4 class
#' @name setPeriodSelector
#' @rdname setPeriodSelector
#' @export
setMethod( f = "setPeriodSelector", signature = c("AmStockChart"),
           definition = function(.Object, periodSelector = NULL, ...)
           {
             if( is.null( periodSelector ) && !missing(...) ){
               periodSelector <- periodSelector(...)
             }else{}
             .Object@periodSelector <- listProperties(periodSelector)
             validObject(.Object)
             return(.Object)
           }
)

# > @setStockEventsSettings : setters ####

#' @exportMethod setStockEventsSettings
setGeneric(name = "setStockEventsSettings", def = function(.Object, ...){ standardGeneric("setStockEventsSettings") } )
#' @title Setter for StockEventsSettings
#' @details Use this methode in case of an AmStockChart.
#' @param \code{.Object}: Object of class \code{\linkS4class{AmStockChart}}.
#' @param \code{...}: Properties of \code{StockEventsSettings}.
#' @return The updated object of class \code{\linkS4class{AmStockChart}}.
#' @examples
#' amStockChart() %>>% setStockEventsSettings(backgroundAlpha = 1)
#' @family AmStockChart setters
#' @family AmStockChart methods
#' @seealso \code{\linkS4class{AmStockChart}} S4 class
#' @name setStockEventsSettings
#' @rdname setStockEventsSettings
#' @export
setMethod( f = "setStockEventsSettings", signature = c("AmStockChart"),
           definition = function(.Object, ...)
           {
             .Object@stockEventsSettings <- list(...)
             validObject(.Object)
             return(.Object)
           }
)

# > @valueAxesSettings: setters ####

#' @exportMethod setValueAxesSettings
setGeneric( name = "setValueAxesSettings",
            def = function(.Object, valueAxesSettings = NULL, ...) {standardGeneric("setValueAxesSettings")} )
#' @title Setter for valueAxesSettings
#' 
#' @param \code{.Object}: Object of class \code{\linkS4class{AmStockChart}}.
#' @param \code{...}: Properties of \code{ValueAxesSettings}.
#' @return The updated object of class \code{\linkS4class{AmStockChart}}.
#' @examples
#' # Setter for valueAxesSettings
#' amStockChart() %>>% setValueAxesSettings(autoGridCount = "TRUE")
#' @family AmStockChart setters
#' @family AmStockChart methods
#' 
#' @name setValueAxesSettings
#' @rdname setValueAxesSettings
#' @export
setMethod( f = "setValueAxesSettings", signature = c("AmStockChart"),
           definition = function(.Object, ...)
           {
             .Object@valueAxesSettings <- list(...)
             validObject(.Object)
             return(.Object)
           }
)

#' @title List attributes of an AmStockChart
#' @description This method lists attributes of an AmChart to fit the API
#' @details For certain attributes we do not verify if they are NULL, see constructor.
#' @examples
#' amChart() %>>% setProperties(test = 1) %>>% listProperties
setMethod( f = "listProperties", signature = "AmStockChart",
           definition = function(.Object)
           {
             ls <- callNextMethod()
             if( length(.Object@categoryAxesSettings) > 0 ){
               ls <- rlist::list.append( ls, categoryAxesSettings = .Object@categoryAxesSettings)
             }else{}
             if( length(.Object@chartCursorSettings) > 0 ){
               ls <- rlist::list.append( ls, chartCursorSettings = .Object@chartCursorSettings)
             }else{}
             if( length(.Object@chartScrollbarSettings) > 0 ){
               ls <- rlist::list.append( ls, chartScrollbarSettings = .Object@chartScrollbarSettings)
             }else{}
             if( length(.Object@comparedDataSets) > 0 ){
               ls <- rlist::list.append( ls, comparedDataSets = .Object@comparedDataSets)
             }else{}
             if( length(.Object@dataSets) > 0 ){
               ls <- rlist::list.append(ls, dataSets = .Object@dataSets)
             }else{}
             if( length(.Object@dataSetSelector) > 0 ){
               ls <- rlist::list.append(ls, dataSetSelector = .Object@dataSetSelector)
             }else{}
             if( length(.Object@legendSettings) > 0 ){
               ls <- rlist::list.append( ls, legendSettings = .Object@legendSettings)
             }else{}
             if( length(.Object@mainDataSet) > 0 ){
               ls <- rlist::list.append( ls, mainDataSet = .Object@mainDataSet)
             }else{}
             if( length(.Object@panels) > 0 ){
               ls <- rlist::list.append( ls, panels = .Object@panels)
             }else{}
             if( length(.Object@panelsSettings) > 0 ){
               ls <- rlist::list.append( ls, panelsSettings = .Object@panelsSettings)
             }else{}
             if( length(.Object@periodSelector) > 0 ){
               ls <- rlist::list.append( ls, panelsSettings = .Object@periodSelector)
             }else{}
             if( length(.Object@periodSelector) > 0 ){
               ls <- rlist::list.append( ls, periodSelector = .Object@periodSelector)
             }else{}
             if( length(.Object@stockEventsSettings) > 0 ){
               ls <- rlist::list.append( ls, stockEventsSettings = .Object@stockEventsSettings)
             }else{}
             if( length(.Object@valueAxesSettings) > 0 ){
               ls <- rlist::list.append( ls, valueAxesSettings = .Object@valueAxesSettings)
             }else{}
             return(ls)
           }
)