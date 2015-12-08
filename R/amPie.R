#' @title Plotting pie chart using rAmCharts
#' @description  amPie computes a pie chart of the given value.
#' @param data \code{data.frame} dataframe with at least 2 columns : 
#' label (character), value (numeric).
#' Your can add a third column "color" (character, colors in hexadecimal).
#' @param main \code{character}, title of the graph.
#' @param mainSize \code{numeric}, size of the title of the graph.
#' @param legend \code{boolean} TRUE to display legend.
#' @param legend_side \code{character} either "left" or "right" if legend is set to TRUE
#' @param show_values \code{boolean} TRUE to display values.
#' @examples
#' amPie(data = data.frame(label = c("Facebook", "Twitter", "LinkedIn", "Google+", 
#'                                        "Pinterest"),
#'                               value = c(38, 25, 15, 14, 8), 
#'                               stringsAsFactors = FALSE), legend = TRUE,
#'             main = "Favourite social media", show_values = TRUE)
#' @export

amPie <- function(data, main = "", mainSize = 15, legend = FALSE, legend_side = NULL,
                  show_values = TRUE) {
  
  if(!is.data.frame(data) | !any(c("label", "value") %in% colnames(data))) {
    stop ("data must be a data frame which at least the columns 'label' (character),
          and 'value' (numeric)")
  } else {}
  
  if(!is.character(data$label)) {
    stop("column 'label' of the dataframe data must be character")
  } else {}
  
  if(!is.numeric(data$value)) {
    stop("column 'value' of the dataframe data must be numeric")
  } else {}
  
  main <- as.character(main)
  
  if(!is.numeric(mainSize)) {
    stop("mainSize must be numeric")
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
  
  if(!"color" %in% colnames(data)) {
    vec_col <- tolower(head(rep(c("#67b7dc", "#fdd400", "#84b761", "#cc4748", 
                                  "#cd82ad", "#2f4074", "#448e4d", "#b7b83f", 
                                  "#b9783f", "#b93e3d", "#913167"), 5), 
                            nrow(data)))
    data$color <- vec_col
  }

  res <- pipeR::pipeline( 
    amPieChart(dataProvider = data, valueField = "value", titleField = "label",
               colorField = "color", labelsEnabled = show_values),
    addTitle(text = main, size = mainSize)
  )
  if(legend) {
    res <- setLegend(res, position = ifelse(is.null(legend_side), 'right',legend_side))
  }
  
  res
}