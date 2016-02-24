#' @title Plotting funnel chart using rAmCharts
#' @description  amFunnel computes a funnel chart of the given value.
#' 
#' @param data a data frame of at least 2 columns : value (numeric, positive), 
#' and description (character). You can add a third column "color" (character,
#' colors in hexadecimal) see \link{data_funnel}.
#' @param neck_height \code{numeric}, value between 0 and 100 : if a bottleneck
#' is desired, this value determines its heigh. Default to NULL.
#' @param neck_width \code{numeric}, value between 0 and 100 : if a bottleneck
#' is desired, this value determines its witdh. Default to NULL.
#' @param depth \code{numeric}, if > 0, chart is displayed in 3D, only for
#' pyramid chart (without a bottleneck). Value between 0 and 100.
#' @param inverse \code{logical}, if TRUE, the funnel chart will be inversed. 
#' @param label_side \code{character}, label position : "right" or "left".
#' @param margin_right \code{numeric}, margin at the right side.
#' @param margin_left \code{numeric}, margin at the left side.
#' @param ... see \code{\link{amOptions}} for more options.
#' 
#' @example examples/amFunnel_examples.R
#'
#' @export

amFunnel <- function(data, inverse = FALSE, neck_height = NULL, neck_width = NULL, 
                     depth = 0, label_side = "right", margin_right = 200,
                     margin_left = 200, ...)
{
  
  .testIn(vect = "description", control = colnames(data))
  .testCharacter(char = data$description, arg = "data$description")
  
  #
  .testIn(vect = "value", control = colnames(data))
  .testNumeric(num = data$value, arg = "data$value")
  
  .testLogical(logi = inverse)
  
  if (!is.null(neck_height)) .testNumeric(num = neck_height)
  
  if (!is.null(neck_width)) .testNumeric(num = neck_width)
  
  .testInterval(num = depth, binf = 0, bsup = 100)
  
  .testNumericLength1(num = margin_right)
  .testNumericLength1(num = margin_left)
  
  if ("color" %in% colnames(data)) {
    .testCharacter(char = data$color)
    vec_col <- data$color
    data <- data[,c("description", "value")]
  } else {
    vec_col <- tolower(head(rep(c("#67b7dc", "#fdd400", "#84b761", "#cc4748", 
                                  "#cd82ad", "#2f4074", "#448e4d", "#b7b83f", 
                                  "#b9783f", "#b93e3d", "#913167"), 5), 
                            nrow(data)))
    data$color <- vec_col
  }
  
  # base chart
  chart <- amFunnelChart(dataProvider = data, titleField = "description", valueField = "value",
                         labelPosition = label_side, marginRight = margin_right, 
                         marginLeft = margin_left, colorField = "color")
  
  if (!is.null(neck_height) & !is.null(neck_width)) {
    neck_height_c <- paste0(neck_height, "%")
    neck_width_c <- paste0(neck_width, "%")
    chart <- setProperties(.Object = chart, neckHeight = neck_height_c, neckWidth = neck_width_c,
                           rotate = inverse, startDuration = 0)
  } else {
    if (depth > 0) {
      chart <- setProperties(.Object = chart, depth3D = depth, angle = 40,
                             rotate = inverse, startDuration = 0)
    } else {
      chart <- setProperties(.Object = chart, rotate = inverse, startDuration = 0)
    }
  }
  
  # add argupment 'RType_' for amOptions
  chart <- setProperties(.Object = chart, RType_ = "funnel")
  amOptions(chart, ...)
}