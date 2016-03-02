#' @title Plotting waterfall chart using rAmCharts
#' @description  amWaterfall computes a waterfall chart of the given value.
#' @param data \code{data.frame} dataframe with at least 3 columns : 
#' label (character), value (numeric), operation (character : "plus", "minus", "total").
#' You can add a third column "color" (character, colors in hexadecimal). You can
#' also add a column "description" (character) containing the text you want to
#' display when mouse is on the graphic ('<br>' for a new line).
#' See \code{\link{data_waterfall}}.
#' @param start \code{numeric} value from which to start
#' @param horiz \code{logical} TRUE for an horizontal chart, FALSE for a vertical one
#' @param show_values \code{logical} TRUE to display values on the chart.
#' @param ... see \code{\link{amOptions}} for more options
#' 
#' @example examples/amWaterfall_examples.R
#' 
#' @export

amWaterfall <- function(data, start = 0, horiz = FALSE,
                        show_values = FALSE, ...) {
  
  
  
  ##Test
  #data
  data <- .testFormatData(data)
  
  #label
  .testIn(vect = "label", control = colnames(data))
  .testCharacter(char = data$label, arg = "data$label")

  #value
  .testIn(vect = "value", control = colnames(data))
  .testNumeric(num = data$value, arg = "data$value")
  
  
  #operation
  .testIn(vect = "operation", control = colnames(data))
  .testCharacter(char = data$operation, arg = "data$operation")
  

  
  .testIn(vect = data$operation, control = c("plus", "minus", "total"))
  
  .testNumericLength1(num = start)
  
  .testLogicalLength1(logi = horiz)

  
  .testLogicalLength1(logi = show_values)
  

  
  if(!"color" %in% colnames(data)) {
    data$color <- ""
    if(any(data$operation == "plus")) {
      data$color[which(data$operation == "plus")] <- "#c1ffc1"
    }
    if(any(data$operation == "minus")) {
      data$color[which(data$operation =="minus")] <- "#ff4040"
    }
    if(any(data$operation == "total")) {
      data$color[which(data$operation == "total")] <- "#1e90ff"
    }
  }
  
  .testCharacter(char = data$color)
  data$color <- toupper(data$color)
  
  data$open[1] <- start
  if(data$operation[1] == "plus") {
    data$open[1] <- start
    data$close[1] <- start + data$value[1]
  } else if (data$operation[1] == "minus") {
    data$open[1] <- start - data$value[1]
    data$close[1] <- start
  } else {
    data$open[1] <- 0
    data$close[1] <- data$value[1]
  }
  
  sapply(2:nrow(data), FUN = function(i) {
    if(data$operation[i] == "total" & data$operation[i-1] != "minus") {
      data$open[i] <<- 0
      data$close[i] <<- data$close[i-1]
      
    } else if(data$operation[i] == "total" & data$operation[i-1] == "minus") {
      data$open[i] <<- 0
      data$close[i] <<- data$open[i-1]
      
    } else if(data$operation[i-1] == "plus" | data$operation[i-1] == "total") {
      if(data$operation[i] == "plus") {
        data$open[i] <<- data$close[i-1]
        data$close[i] <<- data$close[i-1] + data$value[i]
      }else if(data$operation[i] == "minus") {
        data$open[i] <<- data$close[i-1] - data$value[i]
        data$close[i] <<- data$close[i-1]
      }
    } else if(data$operation[i-1] == "minus") {
      if(data$operation[i] == "plus") {
        data$open[i] <<- data$open[i-1]
        data$close[i] <<- data$open[i-1] + data$value[i]
      }else if(data$operation[i] == "minus") {
        data$open[i] <<- data$open[i-1] - data$value[i]
        data$close[i] <<- data$open[i-1]
      }
    }
  })
  
  if("description" %in% colnames(data)) {
    .testCharacter(data$description)
    data$val <- data$description
  } else {
    data$val <- data$value
  }
  data$val2 <- data$value
  
  res <- pipeR::pipeline(
    amSerialChart(dataProvider = data, categoryField = "label", balloonValue = "val",
                  rotate = horiz),
    setProperties(RType_ = "waterfall")
  )
  
  if(show_values) {
    res <- addGraph(res, openField = "open", valueField = "close", colorField = "color",
                    type = "column", lineColor = "#BBBBBB", fillAlphas = 0.8,
                    balloonText =  "<span style='color:[[color]]'>[[label]]</span><br><b>[[val]]</b>",
                    labelText = "[[val2]]")
  } else {
    res <- addGraph(res, openField = "open", valueField = "close", colorField = "color",
                    type = "column", lineColor = "#BBBBBB", fillAlphas = 0.8,
                    balloonText =  "<span style='color:[[color]]'>[[label]]</span><br><b>[[val]]</b>")
  }
  
  sapply(2:nrow(data), FUN = function(i) {
    
    if(data$operation[i-1] != "minus") {
      lig_value <- data$close[i-1]
    } else {
      lig_value <- data$open[i-1]
    }
    res <<- addTrendLine(res, initialCategory = data$label[i-1], 
                         finalCategory = data$label[i],
                         initialValue =lig_value,
                         finalValue = lig_value,
                         lineColor = "#888888", 
                         dashLength = 3)
    
  })
  
  
  res <- amOptions(res, ...)
  res
  
}