.onAttach <- function(...) {
  packageStartupMessage("Important changes: constructors legend() and title() have been replaced by amLegend() and amTitle().")
  packageStartupMessage("For any bug report or feed back see https://github.com/datastorm-open/rAmCharts")
  #packageStartupMessage("\nRemove this message with the following command line: suppressPackageStartupMessages(library(rAmCharts))")
}
