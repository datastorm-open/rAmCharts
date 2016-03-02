#' @title Converts a data.frame in list
#' @description This function is useful for the use of rAmChart, particularly for drill-down feature
#' 
#' @param df \code{data.frame} containing the data.
#' @param keepNA \code{logical}.
#' @examples
#' .toList(data.frame(V1 = c(rep(NA, 5), 6:10), V2 = 11:20, Char = rep("test", 10)), keepNA = FALSE)
#' \dontrun{
#' library(data.table)
#' start <- as.POSIXct("01-01-2015", format = "%d-%m-%Y")
#' end <- as.POSIXct("31-12-2015", format = "%d-%m-%Y")
#' period <- seq.POSIXt(from = start, to = end, by = "10 min")
#' n <- length(period)
#' periodTemp <- seq.POSIXt(from = start, to = end, by = "3 hour")
#' nTemp <- length(periodTemp)
#' ### Generate mesures ----
#' charge <- rnorm(n, mean = 500, sd= 200)
#' charge[ which(charge < 0) ] <- rnorm(length(which(charge < 0)), mean = 200, sd = 10)
#' temp <- rnorm(nTemp, mean = 15, sd = 10)
#' dtCharge <- data.table::data.table(charge, date = period)
#' setkey(dtCharge, date)
#' dtTemp <- data.table::data.table(temperature = temp, date = periodTemp)
#' setkey(dtTemp , date)
#' dp <- dtTemp[dtCharge]
#' dp[ , date := format(date, "%m-%d-%Y %H:%M:%S")]
#' .toList(dp[1:10, ], keepNA = FALSE)
#' }
#' 
#' @details Each row of the data.frame will be transform into a named list.
#' Consequently, be sure that the columns are correctly named.
#' 
#' @import data.table
#' @noRd
#' 
.toList <- function(df, keepNA = TRUE)
{
  if (is(df, "data.table")) { 
    df <- as.data.frame(df)
  } else {}
  lapply(
    X = 1:nrow(df),
    FUN = function(rowID) {
      keepID <- 1:ncol(df)
      if (!keepNA) {
        keepID <- which( !is.na(df[rowID,]) )
      } else {}
      res <- as.list(df[rowID,keepID])
      names(res) <- colnames(df)[keepID]
      return(res)
    })
}

#' @title See AmCharts API
#' @description Open a window in your browser at the referenced documentation
#' under \url{http://docs.amcharts.com/3/javascriptstockchart/}.
#' 
#' @param class
#' Object of class \code{character}.
#' Name of the class to see documentation.
#' Please respect lower and upper case.
#' @examples
#' api()
#' api("AmChart")
#' @export
#' 
api <- function(class = NULL){
  if (is.null(class)) {
    utils::browseURL("http://docs.amcharts.com/3/javascriptstockchart/")
  } else {
    .testCharacterLength1(char = class)
    utils::browseURL(paste0("http://docs.amcharts.com/3/javascriptstockchart/", class))
  }
}