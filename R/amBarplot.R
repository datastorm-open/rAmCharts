#' @title Plotting bar chart using rAmCharts
#' @description  amBarplot computes a bar chart of the given values.
#' 
#' @param x \code{character}, column name for x-axis or \code{numeric} value of the corresponding column.
#' It is optional if argument \code{data} has row names.
#' @param y \code{character}, column name for y-axis or \code{numeric}  vector
#' of the corresponding column. If you want to display a grouped barchart
#' or a stacked barchart, y is a vector of characters or numerics.
#' @param data \code{data.frame} dataframe with values to display.
#' You can add a column "color" (character, colors in hexadecimal). You can
#' also add a column "description" (character) containing the text you want to
#' display when mouse is on the graphic ('<br>' for a new line). 
#' @param groups_color \code{character} vector of colors in hexadecimal, 
#' same length as y.
#' @param xlab \code{character} label for x-axis.
#' @param ylab \code{character} label for y-axis.
#' @param horiz \code{boolean} TRUE for an horizontal chart, FALSE for a vertical one
#' @param labelRotation \code{numeric} Rotation angle of a label. Only horizontal axis' values can be rotated.
#' If you set this for vertical axis, the setting will be ignored. Possible values from -90 to 90.
#' @param stack_type \code{character}, "regular" if you wish stacked bars, "100" if
#' you want 100 percent stacked bars. Default is set to "none".
#' @param layered \code{boolean} TRUE for layered. If TRUE, stack_type must be set
#' to "none".
#' @param show_values \code{boolean} TRUE to display values.
#' @param third_dim \code{boolean} if TRUE, chart is displayed in 3D
#' @param dataDateFormat \code{character}, default 'NULL'. Even if your chart parses dates,
#' you can pass them as strings in your data - 
#' all you need to do is to set data date format and the chart will parse dates to date objects.
#' Check this page for available formats.
#' Please note that two-digit years (YY) as well as literal month names (MMM)  are NOT supported in this setting.
#' @param minPeriod Specifies the shortest period of your data.
#' This should be set only if dataDateFormat is not 'NULL'.
#' Possible period values:
#' fff - milliseconds, ss - seconds, mm - minutes, hh - hours, DD - days, MM - months, YYYY - years.
#' It's also possible to supply a number for increments, i.e. '15mm'
#' which will instruct the chart that your data is supplied in 15 minute increments.
#' 
#' @return An object of class \linkS4class{AmChart}.
#' 
#' @details 
#' \strong{Notice about labels:}
#' if the chart has many columns, several labels might be hidden.
#' It depends on the width of the conatainer where the chart is displayed.
#' Zoom on the chart to see if the chart can contain all labels. If not, use the parameter labelRotation.
#' You can also add a cursor to your chart...
#' 
#' 
#' @example ./examples/amBarplot_examples.R
#' 
#' @export
#' 
amBarplot <- function(x, y, data, xlab = "", ylab = "", groups_color = NULL,horiz = FALSE, labelRotation = 0,
                      stack_type = "none", layered = FALSE, show_values = FALSE, third_dim = FALSE,
                      dataDateFormat = NULL, minPeriod = ifelse(!is.null(dataDateFormat), "DD", ""))
{
  
  if(!is.data.frame(data)) {
    stop("data must be a data frame")
  } else {}
  
  
  # check argument x
  if (missing(x) && !length(rownames(data))) {
    stop("Argument x is not provided and the data.frame does not have row names")
  } else if (missing(x) && length(rownames(data))){
    x <- "xcat_"
    data$xcat_ <- rownames(data)
  } else if (is.character(x) && !(x %in% colnames(data))) {
    stop("Argument x does not correspond to a column name")
  } else if (is.numeric(x) && x > ncol(data)) {
    stop("Error in argument x")
  } else {}
  
  # convert x into character if necessary
  if (is.numeric(x)) x <- colnames(data)[x]
  # check if the column is compatible
  if (!is.character(data[,x]))
    stop(paste("The column ", x, " of the dataframe must be character."))
  
  # check argument y
  if (is.character(y) && !all(y %in% colnames(data)))
    stop(paste("Cannot extract column(s)", y, "from data"))
  
  sapply(1:length(y), FUN = function(i) {
    if (is.numeric(y[i])) {
      if (y[i] > ncol(data)) stop("Error in argument x")
      # convert y into character if necessary
      y[i] <<- colnames(data)[y[i]]
    } else if(is.character(y) && !all(y %in% colnames(data))) {
      stop(paste("Cannot extract column(s)", y, "from data"))
    } else {}
    # check if the column is compatible
    if (!is.numeric(data[,y[i]]))
      stop(paste("The column ", y[i], "of the dataframe must be numeric."))
  })
  
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
  
  if (third_dim) {
    depth3D = 20
    angle = 30
  } else {
    depth3D = 0
    angle = 0
  }
  
  if (show_values) {
    label_text <- "[[value]]"
  } else {
    label_text <- ""
  }
  
  parseDates <- !is.null(dataDateFormat)
  
  res <- pipeR::pipeline(
    amSerialChart(dataProvider = data, categoryField = x, rotate = horiz, 
                  depth3D = depth3D, angle = angle, dataDateFormat = dataDateFormat),
    addValueAxis(title = ylab, position = 'left', stackType = stack_type),
    setCategoryAxis(title = xlab, gridPosition = 'start',
                    labelRotation = labelRotation,
                    axisAlpha = 0, gridAlpha = 0,
                    parseDates = parseDates, minPeriod = minPeriod)
  )
  
  if(length(y) == 1) {
    if("description" %in% colnames(data)) {
      tooltip <- '<b>[[description]]</b>'
    } else {
      tooltip <- '<b>[[value]]</b>'
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
          tooltip2 <- '<b>[[description]]</b>'
        } else {
          tooltip2 <- paste0(as.character(y[i])," : [[value]]")
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
          tooltip2 <- '<b>[[description]]</b>'
        } else {
          tooltip2 <- paste0(as.character(y[i])," : [[value]]")
        }
        res <<- addGraph(res, id = paste0("AmGraph-",i),
                         balloonText = tooltip2, fillColors = v_col[i], legendColor = v_col[i],
                         fillAlphas = 0.85, lineAlpha = 0.1, type = 'column', valueField = y[i],
                         title = y[i], labelText = label_text)
      })
    }
  }
  
  res
  
}

