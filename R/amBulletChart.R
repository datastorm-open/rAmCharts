#' @title Plotting bullet chart using rAmCharts
#' 
#' @description  bulletChart computes a bullet chart of the given value.
#' 
#' @param value \code{numeric}
#' @param min \code{numeric} minimum value allowed
#' @param max \code{numeric} maximum value allowed
#' @param val_color \code{character} color of the bar value, in hexadecimal
#' @param limit \code{numeric} target value
#' @param limit_color \code{character} color of the line limit
#' @param rates a data frame with 4 columns : name (character), min (numeric), max (numeric), 
#' and color (character, color in hexadecimal)
#' @param steps \code{boolean} default set to TRUE
#' @param main \code{character}, title of the graph
#' @param mainSize \code{numeric}, size of the title of the graph.
#' @param legend \code{character} text lengend
#' @param horiz \code{boolean} TRUE for an horizontal bullet chart, FALSE for a vertical one
#' 
#' 
#' @examples
#' 
#' ### basic example
#' # keep the default parameter values: 
#' bulletChart(value = 65)
#' 
#' # equivalent to:
#' bulletChart(value = 65, min = 0, max = 100, val_color = "#000000",
#'              limit = 85, limit_color = "#000000", 
#'              steps = TRUE, main = "Bullet chart", mainSize = 15, 
#'              legend = "", horiz = TRUE,
#'              rates = data.frame(name = c("excelent", "good", "average", "poor", "bad"),
#'                                 min = c(0, 20, 40, 60, 80),
#'                                 max = c(20, 40, 60, 80, 100),
#'                                 color = c("#fb7116", "#b4dd1e", "#f4fb16",
#'                                           "#19d228", "#f6d32b"),
#'                                 stringsAsFactors = FALSE))
#'                                           
#' ### Remove steps for background:
#' bulletChart(value = 65, steps = FALSE)
#' 
#' ### Tune the colors with name or HTML code:
#' bulletChart(value = 65, val_color = "purple", limit_color = "#3c8dbc")
#' 
#' ### Change the orientation:
#' bulletChart(value = 65, steps = FALSE, horiz = FALSE)
#'              
#' ### Add title and legend:
#' bulletChart(value = 65, legend = "Evaluation",
#'             main = "Bullet chart 1", mainSize = 15)
#' 
#' ### Change min and max values:   
#' bulletChart(value = 65, min = 20, max = 90)
#' 
#' @export
#'

bulletChart <- function(value, min = 0, max = 100, val_color = "#000000",
                        limit = 85, limit_color = "#000000", 
                        steps = TRUE, main = "", mainSize = 15, 
                        legend = "", horiz = TRUE, rates) {
  if (missing(rates))
    rates <- data.frame(name = c("excelent", "good", "average", "poor", "bad"),
                        min = c(0, 20, 40, 60, 80),
                        max = c(20, 40, 60, 80, 100),
                        color = c("#19d228", "#b4dd1e", "#f4fb16",
                                  "#f6d32b", "#fb7116"),
                        stringsAsFactors = FALSE)
  
  if(!is.data.frame(rates) | !any(c("name", "min", "max", "color") %in% colnames(bands))) {
    stop ("rates must be a data frame which at least the columns 'name' (numeric),
          'min' (numeric), 'max' (numeric) and 'color' (chararcter, color in hexadecimal)")
  } else {}
  
  if(!is.character(rates$name)) {
    stop("column 'name' of the dataframe rates must be character")
  } else {}
  
  if(!is.numeric(rates$min)) {
    stop("column 'min' of the dataframe rates must be numeric")
  } else {}
  
  if(!is.numeric(rates$max)) {
    stop("column 'max' of the dataframe rates must be numeric")
  } else {}
  
  if(!is.character(rates$color)) {
    stop("column 'color' of the dataframe rates must be character
         (color in hexadecimal)")
  } else {}
  
  if(!is.numeric(value)) {
    stop("value must be numeric")
  } else {}
  
  if(!is.numeric(min)) {
    stop("min must be numeric")
  } else {}
  
  if(!is.numeric(max)) {
    stop("max must be numeric")
  } else {}
  
  if(!is.character(val_color)) {
    stop("val_color must be a character")
  } else {}
  
  if(!is.numeric(limit)) {
    stop("limit must be numeric")
  } else {}
  
  if(!is.character(limit_color)) {
    stop("limit_color must be a character")
  } else {}
  
  if(!is.logical(steps)) {
    stop("steps must be logical")
  } else {}
  
  main <- as.character(main)
  
  if(!is.numeric(mainSize)) {
    stop("mainSize must be numeric")
  } else {}
  
  legend <- as.character(legend)
  
  if(!is.logical(horiz)) {
    stop("horiz must be logical")
  } else {}
  
  val_color <- tolower(val_color)
  limit_color <- tolower(limit_color)
  rates$color <- tolower(rates$color)
  
  dataProvider <- data.frame(category = legend, t(rates$max - rates$min), stringsAsFactors = FALSE)
  colnames(dataProvider)[-1] <- as.character(rates$name)
  dataProvider$limit <- limit
  dataProvider$full <- max
  dataProvider$bullet <- value
  
  # intialize the chart
  amSerialChart(dataProvider = dataProvider, categoryField = "category",
                rotate = horiz, columnWidth = 1) %>>%
    addValueAxes(minimum = min, maximum = max,
                 stackType = "regular", gridAlpha = 0) %>>%
    addTitle(text = main, size = mainSize) %>>%
    addGraph(type = "step", valueField = "limit", columnWidth = 0.5, lineColor = limit_color, 
             lineThickness = 3, noStepRisers = TRUE, stackable = FALSE) %>>%
    (~ chart)
  
  if(steps) {
    sapply(1:nrow(rates), FUN = function(rt) {
      chart <<- addGraph(chart, type = "column", valueField = as.character(rates$name[rt]), fillAlphas = 0.8,
                       lineColor = rates$color[rt], showBalloon = FALSE, columnWidth = 1)
    })
  } else {
    chart <- addGraph(chart, type = "column", valueField = "full", lineAlpha = 0, fillAlphas = 0.8, 
                    fillColors = rates$color, gradientOrientation = ifelse(horiz, "horizontal", "vertical"),
                    showBalloon = FALSE)
  }
  
  chart <- addGraph(chart, type = "column", valueField = "bullet", columnWidth = 0.3, fillAlphas = 1, 
                  lineColor = val_color, clustered = FALSE, stackable = FALSE)
  
  chart
}