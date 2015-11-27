#' @title Plotting funnel chart using rAmCharts
#' @description  amFunnelChart computes a funnel chart of the given value.
#' @param data a data frame of at least 2 columns : value (numeric, positive), 
#' and description (character). Your can add a third column "color" (character,
#' colors in hexadecimal)
#' @param neck_height \code{numeric} value between 0 and 100 : if a bottleneck
#' is desired, this value determines its heigh. Default to NULL
#' @param neck_width \code{numeric} value between 0 and 100 : if a bottleneck
#' is desired, this value determines its witdh. Default to NULL
#' @param third_dim \code{boolean} if TRUE, chart is displayed in 3D, only for
#' pyramid chart (without a bottleneck)
#' @param main \code{character}, title of the graph.
#' @param mainSize \code{numeric}, size of the title of the graph.
#' @param inverse \code{boolean}, if TRUE, the funnel chart will be inversed. 
#' @param label_side \code{character} label position : "right" or "left"
#' @param margin_right \code{numeric} margin at the right side
#' @param margin_left \code{numeric} margin at the left side
#' @examples
#' funnelChart(data = data.frame(description = c("Website visits", "Downloads", 
#'                                                  "Requested price list", 
#'                                                  "Contaced for more info",
#'                                                  "Purchased", "Contacted for support",
#'                                                  "Purchased additional products"), 
#'                                 value = c(300, 123, 98, 72, 80, 15, 8)), 
#'                main = "Pyramid", mainSize = 15, inverse = TRUE)
#'              
#' funnelChart(data = data.frame(description = c("Website visits", "Downloads", 
#'                                                  "Requested price list", 
#'                                                  "Contaced for more info",
#'                                                  "Purchased", "Contacted for support",
#'                                                  "Purchased additional products"), 
#'                                 value = c(300, 123, 98, 72, 80, 15, 8)), 
#'                main = "Reversed Pyramid", mainSize = 15, inverse = FALSE,
#'                label_side = "left", margin_right = 15, margin_left = 160)
#'
#' funnelChart(data = data.frame(description = c("Website visits", "Downloads", 
#'                                                  "Requested price list", 
#'                                                  "Contaced for more info",
#'                                                  "Purchased", "Contacted for support",
#'                                                  "Purchased additional products"), 
#'                                 value = c(300, 123, 98, 72, 80, 15, 8)), 
#'                neck_height = 30, neck_width = 40,
#'                main = "Funnel", mainSize = 15, inverse = FALSE)
#'                
#' funnelChart(data = data.frame(description = c("Website visits", "Downloads", 
#'                                                  "Requested price list", 
#'                                                  "Contaced for more info",
#'                                                  "Purchased", "Contacted for support",
#'                                                  "Purchased additional products"), 
#'                                 value = c(300, 123, 98, 72, 80, 15, 8)), 
#'                neck_height = 30, neck_width = 40,
#'                main = "Reversed Funnel", mainSize = 15, inverse = TRUE,
#'                label_side = "left", margin_right = 15, margin_left = 160)
#'                
#'funnelChart(data = data.frame(description = c("Website visits", "Downloads", 
#'                                                  "Requested price list", 
#'                                                  "Contaced for more info",
#'                                                  "Purchased", "Contacted for support",
#'                                                  "Purchased additional products"), 
#'                                 value = c(300, 123, 98, 72, 80, 15, 8)), 
#'                third_d = TRUE,
#'                main = "Pyramid", mainSize = 15, inverse = TRUE)
#'
#' @export

funnelChart <- function(data, main = "Funnel Chart", mainSize = 15, inverse = FALSE, 
                     neck_height = NULL, neck_width = NULL, third_dim = FALSE,
                     label_side = "right", margin_right = 160, margin_left = 15) {
  if("color" %in% colnames(data)) {
    vec_col <- data$color
    data <- data[,c("description", "value")]
  } else {
    vec_col <- tolower(head(rep(c("#67b7dc", "#fdd400", "#84b761", "#cc4748", 
                                  "#cd82ad", "#2f4074", "#448e4d", "#b7b83f", 
                                  "#b9783f", "#b93e3d", "#913167"), 5), 
                            nrow(data)))
    data$color <- vec_col
  }
  
  if(!is.null(neck_height) & !is.null(neck_width)) {
    neck_height_c <- paste0(neck_height, "%")
    neck_width_c <- paste0(neck_width, "%")
    res <- pipeR::pipeline(
      amFunnelChart(dataProvider = data, titleField = "description", valueField = "value",
                    labelPosition = label_side, marginRight = margin_right, 
                    marginLeft = margin_left, colorField = "color",
                    neckHeight = neck_height_c, neckWidth = neck_width_c,
                    rotate = inverse),
      addTitle(text = main, size = mainSize)
    )
  } else {
    if(third_dim) {
      res <- pipeR::pipeline(
        amFunnelChart(dataProvider = data, titleField = "description", valueField = "value",
                      labelPosition = label_side, marginRight = margin_right, 
                      marginLeft = margin_left, colorField = "color",
                      depth3D = 100, angle = 40,
                      rotate = inverse),
        addTitle(text = main, size = mainSize)
      )
    } else {
      res <- pipeR::pipeline(
        amFunnelChart(dataProvider = data, titleField = "description", valueField = "value",
                      labelPosition = label_side, marginRight = margin_right, 
                      marginLeft = margin_left, colorField = "color",
                      rotate = inverse),
        addTitle(text = main, size = mainSize)
      )
    }
  }
  
  res
}