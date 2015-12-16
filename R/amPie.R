#' @title Plotting pie chart using rAmCharts
#' @description  amPie computes a pie chart of the given value.
#' @param data \code{data.frame} dataframe with at least 2 columns : 
#' label (character), value (numeric).
#' You can add a third column "color" (character, colors in hexadecimal).
#' @param main \code{character}, title of the graph.
#' @param mainSize \code{numeric}, size of the title of the graph.
#' @param legend \code{boolean} TRUE to display legend.
#' @param legend_side \code{character} either "left" or "right" if legend is set to TRUE
#' @param show_values \code{boolean} TRUE to display values.
#' @param third_dim \code{boolean} if TRUE, chart is displayed in 3D
#' @param inner_radius \code{numeric} value between 0 and 100
#' @examples
#' 
#' #Basic example
#' data_pie <- data.frame(label = c("Facebook", "Twitter", "LinkedIn", "Google+", 
#'                                   "Pinterest"),
#'                        value = c(38, 25, 15, 14, 8), stringsAsFactors = FALSE)
#'                        
#' amPie(data = data_pie)
#' 
#' #add legend
#' amPie(data = data_pie, legend = TRUE)
#' 
#' #don't display values
#' amPie(data = data_pie, show_values = FALSE)
#' 
#' #3D pie
#' amPie(data = data_pie, third_dim = TRUE)
#' 
#' #donut chart
#' amPie(data = data_pie, inner_radius = 50)
#' 
#' @export

amPie <- function(data, main = "", mainSize = 15, legend = FALSE, legend_side = NULL,
                  show_values = TRUE, third_dim = FALSE, inner_radius = 0) {
  
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
  
  if(!is.logical(third_dim)) {
    stop("third_dim must be logical")
  } else {}
  
  if(!is.numeric(inner_radius)) {
    stop("inner_radius must be a numeric")
  } else if(inner_radius < 0 | inner_radius > 100) {
    stop("inner_radius must be a numeric between 0 and 100")
  }
  
  if(!"color" %in% colnames(data)) {
    vec_col <- tolower(head(rep(c("#67b7dc", "#fdd400", "#84b761", "#cc4748", 
                                  "#cd82ad", "#2f4074", "#448e4d", "#b7b83f", 
                                  "#b9783f", "#b93e3d", "#913167"), 5), 
                            nrow(data)))
    data$color <- vec_col
  }
  
  if(third_dim) {
    res <- amPieChart(dataProvider = data, valueField = "value", titleField = "label",
               colorField = "color", labelsEnabled = show_values, depth3D = 10,
               angle = 15, innerRadius = paste(inner_radius,"%"))
  } else {
    res <- amPieChart(dataProvider = data, valueField = "value", titleField = "label",
                      colorField = "color", labelsEnabled = show_values,
                      innerRadius = paste(inner_radius,"%"))
  }
  
  res <- addTitle(res, text = main, size = mainSize)
  
  if(legend) {
    res <- setLegend(res, position = ifelse(is.null(legend_side), 'right',legend_side))
  }
  
  res
}