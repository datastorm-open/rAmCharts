#' @title Plotting waterfall chart using rAmCharts
#' @description  amWaterfall computes a waterfall chart of the given value.
#' @param data \code{data.frame} dataframe with at least 3 columns : 
#' label (character), value (numeric), operation (character : "plus", "minus", "total").
#' Your can add a third column "color" (character, colors in hexadecimal).
#' @param start \code{numeric} value from which to start
#' @param main \code{character}, title of the graph
#' @param mainSize \code{numeric}, size of the title of the graph.
#' @param horiz \code{boolean} TRUE for an horizontal bullet chart, FALSE for a vertical one
#' @param show_values \code{boolean} TRUE to display values on the chart.
#' @examples
#' amWaterfall(data = data.frame(label = c("Income 1", "Income 2", "Income 3", "Total 1", 
#'                                        "Expenses 1", "Expenses 2", "Total 2", "Income 4", 
#'                                        "Income 5", "Income 6", "Expenses 3","Total 3", 
#'                                        "Expenses 4", "Expenses 5", "Total 4"),
#'                               value = c(5, 10, 15, 30, 10, 5, 15, 50, 10, 35, 10, 100, 
#'                                         15, 60, 25),
#'                               operation = c(rep("plus", 3), "total", rep("minus", 2),
#'                                             "total", "plus", "minus", rep("plus", 2), 
#'                                             "total", rep("minus", 2), "total"), 
#'                                             stringsAsFactors = FALSE),
#'             main = "Waterfall Example", show_values = TRUE)
#' @export

amWaterfall <- function(data, start = 0, main = "", mainSize = 15, horiz = FALSE,
                        show_values = FALSE) {
  
  if(!is.data.frame(data) | !any(c("label", "value", "operation") %in% colnames(data))) {
    stop ("data must be a data frame which at least the columns 'label' (character),
          'value' (numeric) and 'operation' (character)")
  } else {}
  
  if(!is.character(data$label)) {
    stop("column 'label' of the dataframe data must be character")
  } else {}
  
  if(!is.numeric(data$value)) {
    stop("column 'value' of the dataframe data must be numeric")
  } else {}
  
  if(!is.character(data$operation)) {
    stop("column 'operation' of the dataframe data must be character")
  } else {}
  
  if(!any(data$operation %in% c("plus", "minus", "total"))) {
    stop("column 'operation' of the dataframe data must only contain
          'plus', 'minus' and 'total'")
  } else {}
  
  if(!is.numeric(start)) {
    stop("start must be numeric")
  } else {}
  
  main <- as.character(main)
  
  if(!is.numeric(mainSize)) {
    stop("mainSize must be numeric")
  } else {}
  
  if(!is.logical(horiz)) {
    stop("horiz must be logical")
  } else {}
  
  if(!is.logical(show_values)) {
    stop("show_values must be logical")
  } else {}
  
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
  data$val <- data$value
  
  res <- pipeR::pipeline(
    amSerialChart(dataProvider = data, categoryField = "label", balloonValue = "val",
                  rotate = horiz),
    addTitle(text = main, size = mainSize)
  )
  
  if(show_values) {
    res <- addGraph(res, openField = "open", valueField = "close", colorField = "color",
                    type = "column", lineColor = "#BBBBBB", fillAlphas = 0.8,
                    balloonText =  "<span style='color:[[color]]'>[[label]]</span><br><b>[[val]]</b>",
                    labelText = "[[val]]")
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
  
  res
}