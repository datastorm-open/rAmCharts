#' @title Plotting mekko chart (quali vs quali) using rAmCharts
#' @description  amMekko computes a mekko chart of the given values.
#' @param x \code{character}, column name for x-axis.
#' @param y \code{character}, column name for y-axis.
#' @param data \code{data.frame}, dataframe with values to display.
#' See \code{\link{data_mekko}}
#' @param groups_color \code{character} vector of colors in hexadecimal, 
#' same length as the number of y modalities.
#' @param xlab \code{character}, label for x-axis.
#' @param ylab \code{character}, label for y-axis.
#' @param horiz \code{logical}, TRUE for an horizontal chart, FALSE for a vertical one.
#' @param show_values \code{logical}, TRUE to display values.
#' @param ... see \code{\link{amOptions}} for more options.
#' 
#' @examples
#' data(data_mekko)
#' amMekko(x = "var1", y = "var2", data = data_mekko)
#' 
#' \donttest{
#' # Other examples available which can be time consuming depending on your configuration.
#' library(pipeR)
#'  
#' # Horizontal
#' amMekko(x = "var1", y = "var2", data = data_mekko, horiz = TRUE)
#'  
#' # Display values
#' amMekko(x = "var1", y = "var2", data = data_mekko, show_values = TRUE)
#' }
#' 
#' @seealso 
#' \itemize{
#' \item{\url{https://datastorm-open.github.io/introduction_ramcharts/}}
#' }
#' 
#' @export
amMekko <- function(x, y, data, xlab = "", 
                    ylab = "", groups_color = NULL,
                    horiz = FALSE, show_values = FALSE, ...)
{
  x <- match.arg(arg = x, choices = colnames(data))
  y <- match.arg(arg = y, choices = colnames(data))
  
  # data
  data[,x] <- as.factor(as.character(data[, x]))
  data[,y] <- as.factor(as.character(data[, y]))
  
  .testCharacterLength1(char = xlab)
  .testCharacterLength1(char = ylab)
  .testLogicalLength1(logi = horiz)
  
  
  .testLogicalLength1(logi = show_values)
  
  
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
    setCategoryAxis(title = xlab, gridPosition = 'start',
                    axisAlpha = 0, gridAlpha = 0, widthField = "total")
  )
  
  if(!is.null(groups_color)) {
    .testCharacter(char = groups_color)
    if(length(groups_color) == length(y)) {
      v_col <- groups_color
    } else {
      v_col <- utils::head(rep(c("#67b7dc", "#fdd400", "#84b761", "#cc4748", 
                                 "#cd82ad", "#2f4074", "#448e4d", "#b7b83f", 
                                 "#b9783f", "#b93e3d", "#913167"), 5), 
                           length(y))
    }
  } else {
    v_col <- utils::head(rep(c("#67b7dc", "#fdd400", "#84b761", "#cc4748", 
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
  
  res <- amOptions(res, ...)
  res
}

