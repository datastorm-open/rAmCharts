#' @title Plotting mekko chart (quali vs quali) using rAmCharts
#' @description  amMekko computes a mekko chart of the given values.
#' @param x \code{character} column name for x-axis or \code{numeric} 
#' the number of the corresponding column.
#' @param y \code{character} column name for y-axis or \code{numeric} 
#' the number of the corresponding column. 
#' @param data \code{data.frame} dataframe with values to display.
#' @param groups_color \code{character} vector of colors in hexadecimal, 
#' same length as the number of y modalities.
#' @param main \code{character} title of the graph.
#' @param mainSize \code{numeric} size of the title of the graph.
#' @param xlab \code{character} label for x-axis.
#' @param ylab \code{character} label for y-axis.
#' @param horiz \code{boolean} TRUE for an horizontal chart, FALSE for a vertical one
#' @param legend \code{boolean} TRUE to display legend.
#' @param legend_side \code{character} either "bottom", "top", "left" or "right" if 
#' legend is set to TRUE.
#' @param show_values \code{boolean} TRUE to display values.
#' @examples
#' 
#' 
#' #Basic Example : 
#' data_mekko <- data.frame(var1 = c(rep("A1", 150), rep("A2", 350), rep("A3", 500)),
#'                          var2 = sample(c("B1", "B2", "B3", "B4", "B5", "B6"), 
#'                                        1000, replace = TRUE))
#'                                        
#' amMekko(x = "var1", y = "var2", data = data_mekko)
#' 
#' #Horizontal :
#' amMekko(x = "var1", y = "var2", data = data_mekko, horiz = TRUE)
#' 
#' #display values :
#' amMekko(x = "var1", y = "var2", data = data_mekko, show_values = TRUE)
#' 
#' #add legend :
#' amMekko(x = "var1", y = "var2", data = data_mekko, legend = TRUE)
#' 
#' @export


amMekko <- function(x, y, data, main = "", mainSize = 15, xlab = "", 
                    ylab = "", groups_color = NULL, horiz = FALSE,
                    legend = FALSE, legend_side = "right", show_values = FALSE) {
  
  if(!is.data.frame(data) | (!any(c(x, y) %in% colnames(data)) & is.character(x) & is.character(y))) {
    stop("data must be a data frame")
  } else {}
  
  data[,x] <- as.factor(as.character(data[,x]))
  data[,y] <- as.factor(as.character(data[,y]))
  
  if(is.numeric(x)) {
    x <- colnames(data)[x]
  }
  
  if(is.numeric(y)) {
    y <- colnames(data)[y]
  }
  
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
  
  if(!is.logical(horiz)) {
    stop("horiz must be logical.")
  } else {}
  
  if(!is.logical(legend)) {
    stop("legend must be logical")
  } else {}
  
  if(!is.null(legend_side)) {
    if(!is.character(legend_side) | !legend_side %in% c("bottom", "top", "left", "right")) {
      stop("legend side must be character, either 'bottom', 'top', 'left' or 'right'")
    } 
  }
  
  if(legend & is.null(legend_side)) {
    legend_side <- "bottom"
  }
  
  if(!is.logical(show_values)) {
    stop("show_values must be logical")
  } else {}
  
  if(show_values) {
    label_text <- "[[value]]"
  } else {
    label_text <- ""
  }
  
  tab1 <- as.data.frame.matrix(table(data[,c(x,y)]))
  tab1$total <- rowSums(tab1)
  tab1$abs <- rownames(tab1)
  
  res <- pipeR::pipeline(
    amSerialChart(dataProvider = tab1, categoryField = "abs", rotate = horiz,
                  columnWidth = 1),
    addValueAxis(title = ylab, position = 'left', stackType = "100% stacked",
                 unit = "%", axisAlpha = 0, gridAlpha = 0.1),
    setCategoryAxis(title = xlab, gridPosition = 'start', labelRotation = 45, 
                    axisAlpha = 0, gridAlpha = 0, widthField = "total")
  )
  
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
  
  sapply(1:(ncol(tab1)-2), FUN = function(i) {
    valcol <- colnames(tab1)[i]
    res <<- addGraph(res, id = paste0("AmGraph-",i),
                     balloonText = paste0('<b>[[category]] - ',valcol,'</b> \n[[value]]'), fillColors = v_col[i], 
                     legendColor = v_col[i],
                     fillAlphas = 0.8, lineAlpha = 0, type = 'column', valueField = valcol,
                     title = valcol, labelText = label_text)
  })
  
  if(legend) {
    res <- setLegend(res, position = legend_side, useGraphSettings = TRUE)
  }
  
  res
  
}

