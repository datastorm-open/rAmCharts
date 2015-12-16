#' @title Plotting bar chart using rAmCharts
#' @description  amBar computes a pie chart of the given value.
#' @param x \code{character} column name for x-axis or \code{numeric} 
#' the number of the corresponding column.
#' @param y \code{character} column name for y-axis or \code{numeric} 
#' the number of the corresponding column. If you want to display a grouped barchart
#' or a stacked barchart, y is a vector of characters or numerics. 
#' @param data \code{data.frame} dataframe with values to display.
#' You can add a column "color" (character, colors in hexadecimal). You can
#' also add a column "description" (character) containing the text you want to
#' display when mouse is on the graphic ('<br>' for a new line). 
#' @param groups_color \code{character} vector of colors in hexadecimal, 
#' same length as y.
#' @param main \code{character} title of the graph.
#' @param mainSize \code{numeric} size of the title of the graph.
#' @param xlab \code{character} label for x-axis.
#' @param ylab \code{character} label for y-axis.
#' @param horiz \code{boolean} TRUE for an horizontal chart, FALSE for a vertical one
#' @param stack_type \code{character}, "regular" if you wish stacked bars, "100" if
#' you want 100 percent stacked bars. Default is set to "none".
#' @param layered \code{boolean} TRUE for layered. If TRUE, stack_type must be set
#' to "none".
#' @param legend \code{boolean} TRUE to display legend.
#' @param legend_side \code{character} either "left" or "right" if legend is set to TRUE.
#' @param show_values \code{boolean} TRUE to display values.
#' @param third_dim \code{boolean} if TRUE, chart is displayed in 3D
#' @examples
#' 
#' 
#' #Basic Example : column chart
#' data_bar <- data.frame(country = c("USA", "China", "Japan", "Germany", 
#'                                     "UK", "France", "India", "Spain",
#'                                     "Netherlands", "Russia", "South Korea",
#'                                     "Canada"),
#'                         visits = c(3025, 1882, 1809, 1322, 1122, 1114, 
#'                                    984, 711, 665, 580, 443, 441),
#'                         color = c("#FF0F00", "#FF6600", "#FF9E01", "#FCD202",
#'                                   "#F8FF01", "#B0DE09", "#04D215", "#0D8ECF",
#'                                   "#0D52D1", "#2A0CD0", "#8A0CCF", "#CD0D74"),
#'                         stringsAsFactors = FALSE)
#' amBar(x = "country", y = "visits", data = data_bar)
#' 
#' #horizontal bar
#' amBar(x = "country", y = "visits", data = data_bar, horiz = TRUE)
#' 
#' #3D bar
#' amBar(x = "country", y = "visits", data = data_bar, third_dim = TRUE)
#' 
#' #display values
#' amBar(x = "country", y = "visits", data = data_bar, show_values = TRUE)
#'
#' #grouped columns
#' data_gbar <- data.frame(year = c("2005", "2006", "2007", "2008", "2009"),
#'                         income = c(23.5, 26.2, 30.1, 29.5, 24.6),
#'                         expenses = c(18.1, 22.8, 23.9, 25.1, 25),
#'                         stringsAsFactors = FALSE)
#' 
#' amBar(x = "year", y = c("income", "expenses"), data = data_gbar)
#' 
#' #add legend
#' amBar(x = "year", y = c("income", "expenses"), data = data_gbar, legend = TRUE)
#' 
#' #change groups colors
#' amBar(x = "year", y = c("income", "expenses"), data = data_gbar, 
#'       groups_color = c("#87cefa", "#c7158"), legend = TRUE)
#' 
#' #stacked bars
#' amBar(x = "year", y = c("income", "expenses"), data = data_gbar, stack_type = "regular")
#' 
#' #100% stacked bars
#' amBar(x = "year", y = c("income", "expenses"), data = data_gbar, stack_type = "100")
#' 
#' #layered bars
#' amBar(x = "year", y = c("income", "expenses"), data = data_gbar, layered = TRUE)
#' 
#' 
#' @export


amBar <- function(x, y, data, main = "", mainSize = 15, xlab = "", 
                  ylab = "", groups_color = NULL, horiz = FALSE, stack_type = "none",
                  layered = FALSE,
                  legend = FALSE, legend_side = "right", show_values = FALSE,
                  third_dim = FALSE) {
  
  if(!is.data.frame(data) | !any(c(x, y) %in% colnames(data))) {
    stop("data must be a data frame")
  } else {}
  
  if(!is.character(data[,x])) {
    stop(paste("The column ", x, " of the dataframe must be character."))
  } else {}
  
  if(is.numeric(x)) {
    x <- colnames(data)[x]
  }
  
  sapply(1:length(y), FUN = function(i) {
    if(!is.numeric(data[,y[i]])) {
      stop(paste("The column ", y[i], "of the dataframe must be numeric."))
    }
  })
  
  sapply(1:length(y), FUN = function(i) {
    if(is.numeric(y[i])) {
      y[i] <<- colnames(data)[y[i]]
    }
  })
  
  main <- as.character(main)
  
  if(!is.numeric(mainSize)) {
    stop("mainSize must be numeric.")
  } else {}
  
  if(!is.character(xlab)) {
    stop("xlab must be character.")
  } else {}
  
  if(!is.character(ylab)) {
    stop("ylab must be character.")
  } else {}
  
  if(!is.logical(layered)) {
    stop("layered must be logical.")
  } else {}
  
  if(layered & stack_type != "none") {
    stop("You have to choose : layered or stacked. If layered
         is set to TRUE, stack_type must be equal to 'none'")
  }
  
  if(!is.logical(horiz)) {
    stop("horiz must be logical.")
  } else {}
  
  if(!is.logical(legend)) {
    stop("legend must be logical")
  } else {}
  
  if(!is.null(legend_side)) {
    if(!is.character(legend_side) | !legend_side %in% c("left", "right")) {
      stop("legend side must be character, either 'left' or 'right'")
    } 
  }
  
  if(!is.logical(show_values)) {
    stop("show_values must be logical")
  } else {}
  
  if(!is.logical(third_dim)) {
    stop("third_dim must be logical")
  } else {}
  
  if(!is.null(stack_type)) {
    if(!is.character(stack_type) | !stack_type %in% c("regular", "100", "none")) {
      stop("stack_type must be a character, either 'regular', '100' or 'none'")
    }
  }
  
  if(stack_type == "100") stack_type = "100%"
  
  if(!"color" %in% colnames(data)) {
    if(length(y) == 1) {
      if(!is.null(groups_color)) {
        data$color <- groups_color[1]
      } else {
        vec_col <- tolower(head(rep(c("#67b7dc", "#fdd400", "#84b761", "#cc4748", 
                                      "#cd82ad", "#2f4074", "#448e4d", "#b7b83f", 
                                      "#b9783f", "#b93e3d", "#913167"), 5), 
                                nrow(data)))
        data$color <- vec_col
      }
    } 
  }
  
  if(third_dim) {
    depth3D = 20
    angle = 30
  } else {
    depth3D = 0
    angle = 0
  }
  
  if(show_values) {
    label_text <- paste0('[[', y, ']]')
  } else {
    label_text <- ""
  }
  
  res <- pipeR::pipeline(
    amSerialChart(dataProvider = data, categoryField = x, rotate = horiz, 
                  depth3D = depth3D, angle = angle),
    addValueAxis(title = ylab, position = 'left', stackType = stack_type),
    setChartCursor(categoryBalloonEnabled = FALSE, cursorAlpha = 0),
    setCategoryAxis(title = xlab, gridPosition = 'start', labelRotation = 45, 
                    axisAlpha = 0, gridAlpha = 0)
  )
  
  if(length(y) == 1) {
    if("description" %in% colnames(data)) {
      tooltip <- '<b>[[category]]: [[description]]</b>'
    } else {
      tooltip <- '<b>[[category]]: [[value]]</b>'
    }
    res <- addGraph(res, balloonText = tooltip, fillColorsField = 'color', 
                    fillAlphas = 0.85, lineAlpha = 0.1, type = 'column', valueField = y,
                    labelText = label_text)
  } else if(length(y) > 1) {
    if(!is.null(groups_color)) {
      if(length(groups_color) == length(y)) {
        v_col <- groups_color
      } else {
        v_col <- head(rep(c("#67b7dc", "#fdd400", "#84b761", "#cc4748", 
                            "#cd82ad", "#2f4074", "#448e4d", "#b7b83f", 
                            "#b9783f", "#b93e3d", "#913167"), 5), 
                      length(y))
      }
    } else {
      v_col <- head(rep(c("#67b7dc", "#fdd400", "#84b761", "#cc4748", 
                          "#cd82ad", "#2f4074", "#448e4d", "#b7b83f", 
                          "#b9783f", "#b93e3d", "#913167"), 5), 
                    length(y))
    }
    
    if(layered) {
      col_height <- rep(1, length = length(y))
      sapply(2:length(col_height), FUN = function(i) {
        col_height[i] <<- col_height[i-1]/2
      })
      col_height[1] <- 0.9
      sapply(1:length(y), FUN = function(i) {
        if("description" %in% colnames(data)) {
          tooltip2 <- '<b>[[category]]: [[description]]</b>'
        } else {
          tooltip2 <- paste0("<b>[[category]]</b> : <br>", as.character(y[i])," : [[value]]")
        }
        res <<- addGraph(res, id = paste0("AmGraph-",i),
                         balloonText = tooltip2, fillColors = v_col[i], legendColor = v_col[i],
                         fillAlphas = 0.85, lineAlpha = 0.1, type = 'column', valueField = y[i],
                         title = y[i], labelText = label_text, clustered = FALSE, 
                         columnWidth = col_height[i])
      })
    } else {
      sapply(1:length(y), FUN = function(i) {
        if("description" %in% colnames(data)) {
          tooltip2 <- '<b>[[category]]: [[description]]</b>'
        } else {
          tooltip2 <- paste0("<b>[[category]]</b> : <br>", as.character(y[i])," : [[value]]")
        }
        res <<- addGraph(res, id = paste0("AmGraph-",i),
                         balloonText = tooltip2, fillColors = v_col[i], legendColor = v_col[i],
                         fillAlphas = 0.85, lineAlpha = 0.1, type = 'column', valueField = y[i],
                         title = y[i], labelText = label_text)
      })
    }
  }
  
  if(legend) {
    res <- setLegend(res, position = ifelse(is.null(legend_side), 'right',legend_side))
  }

  res
  
}

