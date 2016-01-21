#' @title Plotting floating bar chart using rAmCharts
#' @description  amFloatingBar computes a floating bar chart of the given values.
#' 
#' @param x \code{character}, column name for x-axis or \code{numeric} value of the corresponding column.
#' It is optional if argument \code{data} has row names.
#' @param y_inf \code{character}, column name for the lower value or \code{numeric}  vector
#' of the corresponding column. 
#' @param y_sup \code{character}, column name for the upper value or \code{numeric}  vector
#' of the corresponding column. 
#' @param data \code{data.frame} dataframe with values to display.
#' You can add a column "color" (character, colors in hexadecimal). You can
#' also add a column "description" (character) containing the text you want to
#' display when mouse is on the graphic ('<br>' for a new line).
#' See \code{\link{data_fbar}}
#' @param groups_color \code{character} vector of colors in hexadecimal, 
#' same length as y_inf or y_sup.
#' @param xlab \code{character} label for x-axis.
#' @param ylab \code{character} label for y-axis.
#' @param horiz \code{boolean} TRUE for an horizontal chart, FALSE for a vertical one
#' @param labelRotation \code{numeric} Rotation angle of a label. Only horizontal axis' values can be rotated.
#' If you set this for vertical axis, the setting will be ignored. Possible values from -90 to 90.
#' @param show_values \code{boolean} TRUE to display values.
#' @param depth \code{numeric} if > 0, chart is displayed in 3D. Value between 0 and 100.
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
#' @param ... see \code{\link{amOptions}} for more options
#' @param stack_type ...
#' @param layered ...
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
#' @example ./examples/amFloatingBar_examples.R
#' 
#' @export
#' 
amFloatingBar <- function(x, y_inf, y_sup, data, xlab = "", ylab = "", groups_color = NULL,horiz = FALSE, labelRotation = 0,
                          stack_type = "none", layered = FALSE, show_values = FALSE, depth = 0,
                          dataDateFormat = NULL, minPeriod = ifelse(!is.null(dataDateFormat), "DD", ""), ...)
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
  
  # check argument y_inf
  if (is.character(y_inf) && !all(y_inf %in% colnames(data)))
    stop(paste("Cannot extract column(s)", y_inf, "from data"))
  
  sapply(1:length(y_inf), FUN = function(i) {
    if (is.numeric(y_inf[i])) {
      if (y_inf[i] > ncol(data)) stop("Error in argument x")
      # convert y_inf into character if necessary
      y_inf[i] <<- colnames(data)[y_inf[i]]
    } else if(is.character(y_inf) && !all(y_inf %in% colnames(data))) {
      stop(paste("Cannot extract column(s)", y_inf, "from data"))
    } else {}
    # check if the column is compatible
    if (!is.numeric(data[,y_inf[i]]))
      stop(paste("The column ", y_inf[i], "of the dataframe must be numeric."))
  })
  
  # check argument y_sup
  if (is.character(y_sup) && !all(y_sup %in% colnames(data)))
    stop(paste("Cannot extract column(s)", y_sup, "from data"))
  
  sapply(1:length(y_sup), FUN = function(i) {
    if (is.numeric(y_sup[i])) {
      if (y_sup[i] > ncol(data)) stop("Error in argument x")
      # convert y_sup into character if necessary
      y_sup[i] <<- colnames(data)[y_sup[i]]
    } else if(is.character(y_sup) && !all(y_sup %in% colnames(data))) {
      stop(paste("Cannot extract column(s)", y_sup, "from data"))
    } else {}
    # check if the column is compatible
    if (!is.numeric(data[,y_sup[i]]))
      stop(paste("The column ", y_sup[i], "of the dataframe must be numeric."))
  })
  
  .testCharacterLength1(char = xlab)
  .testCharacterLength1(char = ylab)
  .testLogicalLength1(logi = layered)
  
  if(layered && stack_type != "none") {
    stop("You have to choose : layered or stacked. If layered
         is set to TRUE, stack_type must be equal to 'none'")
  }
  
  .testLogicalLength1(logi = horiz)
  .testLogicalLength1(logi = show_values)
  .testInterval(num = depth, binf = 0, bsup = 100)
  
  
  if(!is.null(stack_type)) {
    .testCharacter(char = stack_type)
    .testIn(vect = stack_type, control = c("regular", "100", "none"))
  }
  
  if(stack_type == "100") stack_type = "100%"
  
  if(!"color" %in% colnames(data)) {
    if(length(y_inf) == 1) {
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
  
  if (depth > 0) {
    depth3D = depth
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
  
  data$tp_val <- data[,y_sup] - data[,y_inf]
  
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
  
  if("description" %in% colnames(data)) {
    tooltip <- '<b>[[description]]</b>'
  } else {
    tooltip <- '<b>[[tp_val]]</b>'
  }
  res <- addGraph(res, balloonText = tooltip, fillColorsField = 'color', 
                  fillAlphas = 0.85, lineAlpha = 0.1, type = 'column', valueField = y_sup,
                  openField = y_inf, labelText = label_text)
  
  res <- amOptions(res, ...)
  res
}

