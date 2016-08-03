#' @title Plotting Gantt chart
#' @description  amGantt compute a Gantt chart, still in dev
#' 
#' @param data \code{data.frame}, dataframe with values to display.
#' You must add column category, should be date.
#' You must add column begin, should be date.
#' You can add a column "color" (character, colors in hexadecimal). You can
#' also add a column "description" (character) containing the text you want to
#' display when mouse is on the graphic ('<br>' for a new line).
#' You can add a column to specify legend label
#' @param horiz \code{logical}, TRUE for an horizontal chart, FALSE for a vertical one
#' @param firstdate \code{date}, Date to initialing chart, (minimum), if null take value : min(data[,"begin"])
#' @param ... see \code{\link{amOptions}} for more options.
#' 
#' 
#' @examples
#' data(data_gantt)
#' 
#' amGantt(data_gantt)
#' amGantt(data_gantt, zoom = TRUE)
#' data_gantt$legend <- c("category 1", "category 2", "category 1", "category 2", "category 1")
#' amGantt(data_gantt, valuescrollbar = TRUE)
#' amGantt(data_gantt, valuescrollbar = TRUE, valuescrollbarHeight = 50)
#' @export
amGantt <- function(data, horiz = TRUE, datelim = NULL, ...){
  
  

  if(is.null(datelim)){
    datelim <- as.Date(c("0000-01-01", "0000-01-01"))
    datelim[1] = min(data$begin)
    datelim[2] = max(data$end)
  }
  datelim <- as.Date(datelim)
  if(is.null(data$description)){
    data$description <- paste0('<p style="font-size:15px"><b> ',data$category ,"</p></b>", "From : ",
                          data$begin, "<br>To : ",data$end)
    
  }
  
  if(is.null(data$color)){
    data$color <- "#0000FF"
  }
  
  
  data_end <- lapply(unique(data$category), function(X){
    data1 <- data[which(X == data$category),]
    select <- data.frame(category = data1$category)
    select$begin <- as.numeric(data1$begin - datelim[1])
    select$end <- as.numeric(data1$end - data1$begin)
    select$color <- data1$color
    select$description <- data1$description
    select
  })

    period = "DD"
    
    
  
  chart <- amGanttChart(brightnessStep = 20,
                rotate = horiz,
               startField = "begin",
               segmentsField = "segments",
               dataDateFormat = "YYYY-MM-DD",
               startDate = as.character(datelim[1]),
               period = period,
               categoryField = "category",
               durationField = "end",
               colorField = "color") %>>%
    setGraph(amGraph(fillAlphas = 0.8,
                     balloonText = "[[description]]")) %>>%
    setValueAxis(valueAxis(type = "date",
                           minimumDate = as.character(datelim[1]),
                 maximumDate = as.character(datelim[2]))) %>>%
    setDataProvider(data.frame(category = as.character(unique(data$category) ))) %>>%
    addSegment(1:length(data_end), data_end) 
  
  
  if("legend" %in% names(data)){
    datalegend <- unique(data.frame(color = as.character(data_gantt$color),
                                    legend = data_gantt$legend))
    datalegend <- unname(apply(datalegend, 1, function(X){
      title = as.character(X["legend"])
      color = as.character(X["color"])
      list(title = title, color = color)
    }))
    
    chart <- chart %>>% setLegend(data = datalegend)
  }
  
  
  
  
  
  
  chart@otherProperties$RType_ <- "gantt"
  amOptions(chart, ...)
   
}
