#' @title Plotting pie chart using rAmCharts
#' @description  amPie computes a pie chart of the given value.
#' @param data \code{data.frame} dataframe with at least 2 columns : 
#' label (character), value (numeric).
#' You can add a third column "color" (character, colors in hexadecimal).
#' @param show_values \code{boolean} TRUE to display values.
#' @param third_dim \code{boolean} if TRUE, chart is displayed in 3D
#' @param inner_radius \code{numeric} value between 0 and 100
#' 
#' @example examples/amPie_examples.R
#' 
#' @rdname amPie
#'     
#' @export

amPie <- function(data,show_values = TRUE, third_dim = FALSE, inner_radius = 0) {
  
  
  ##Test
  #data
  #label
  .testIn("label", colnames(data))
  .testCharacter(data$label, arg = "data$label")
  
  #label
  .testIn("value", colnames(data))
  .testNumeric(data$value, arg = "data$value")
  
  .testLogicalLength1(show_values)
  .testLogicalLength1(third_dim)

  .testNumericLength1(inner_radius)
  .testInterval(inner_radius, binf = 0, bsup = 100)
  
  if(!"color" %in% colnames(data)) {
   
    vec_col <- tolower(head(rep(c("#67b7dc", "#fdd400", "#84b761", "#cc4748", 
                                  "#cd82ad", "#2f4074", "#448e4d", "#b7b83f", 
                                  "#b9783f", "#b93e3d", "#913167"), 5), 
                            nrow(data)))
    data$color <- vec_col
  }else{
    .testCharacter(data$color)
  }
  
  if(third_dim) {
    res <- amPieChart(dataProvider = data, valueField = "value", titleField = "label", startDuration = 0,
                      colorField = "color", labelsEnabled = show_values, depth3D = 10,
                      angle = 15, innerRadius = paste(inner_radius,"%"))
  } else {
    res <- amPieChart(dataProvider = data, valueField = "value", titleField = "label", startDuration = 0,
                      colorField = "color", labelsEnabled = show_values,
                      innerRadius = paste(inner_radius,"%"))
  }
  
  res
}