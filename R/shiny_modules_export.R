#' Shiny module to export rAmCharts graphics on server-side
#'
#' This function need the \code{base64enc} package to save image.
#' 
#' @param id  character, used to specify namesapce, see \code{shiny::\link[shiny]{NS}}
#' @param input   standard, \code{shiny} input
#' @param output  standard, \code{shiny} output
#' @param session standard, \code{shiny} session
#' @param list_am_graph named list, reactive expression with all amCharts to export
#' \itemize{
#'  \item{"graph"}{rAmCharts object to export}
#'  \item{"name"}{character, name of file, with ".jpg" extension}
#'  \item{"width"}{Optionnal, character. Linked to \link{amChartsOutput}}
#'  \item{"height"}{Optionnal, character. Linked to \link{amChartsOutput}}
#'  \item{"type"}{Optionnal, character. Linked to \link{amChartsOutput}}
#'}
#' @param path character, directory
#' 
#' @name rAmCharts-shinymodules
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' # ui
#' rAmChartExportServerUI("export_server_graphs")
#' 
#' # server
#'
#'mult_amgraph <- reactive({
#'  if(input$goSave > 0){
#'    isolate({
#'      list(
#'        list(graph = amPie(data = data_pie), name = "pie.jpg", height = "200px", width = "100px"),
#'        list(graph = amBarplot(x = "country", y = "visits", data = data_bar, main = "example") %>%
#'               setExport(), name = "bar.jpg", height = "200px")
#'      )
#'    })
#'  } else {
#'    NULL
#'  }
#'})
#'
#'
#'callModule(rAmChartExportServer, "export_server_graphs", mult_amgraph, 
#'           reactive("/home/benoit/amchart_export"))
#' 
#' }
#' @export
rAmChartExportServerUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("racems_graphs"))
}

#' @rdname rAmCharts-shinymodules
#' @export
rAmChartExportServer <- function(input, output, session, list_am_graph, path){
  
  ns <- session$ns
  
  # plusieurs graphiques
  shiny::observe({
    list_chart <- list_am_graph()
    shiny::isolate({
      # ui
      args_amco <- formals(amChartsOutput)
      to_ui <- lapply(1:length(list_chart), function(x){
        if("width" %in% names(list_chart[[x]])){
          width <- list_chart[[x]]$width
        } else {
          width <- args_amco$width
        }
        
        if("height" %in% names(list_chart[[x]])){
          height <- list_chart[[x]]$height
        } else {
          height <- args_amco$height
        }
        
        if("type" %in% names(list_chart[[x]])){
          type <- list_chart[[x]]$type
        } else {
          type <- args_amco$type
        }
        
        .rAmChartExportServerUI(ns(paste0("racems_graph_", x)), 
                                width = width, height = height, type = type)
      })
      
      output$racems_graphs <- shiny::renderUI({
        shiny::fluidRow(
          do.call(shiny::tagList, to_ui)
        )
      })
      
      # server
      all_path_file <- sapply(1:length(list_chart), function(x){
        shiny::callModule(.rAmChartExportServer, paste0("racems_graph_", x), 
                          shiny::reactive(list_chart[[x]]$graph), 
                          path, shiny::reactive(list_chart[[x]]$name))
      })
    })
  })
  
  return()
}

.rAmChartExportServerUI <- function(id,  ...) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(shiny::selectInput(ns("racems_img64"), NULL,choices = NULL), style = "display:none"),
    shiny::div(amChartsOutput(ns("racems_graph"), ...))
  )
}


.rAmChartExportServer <- function(input, output, session, am_graph, 
                                  path = shiny::reactive(getwd()), name = shiny::reactive("amChart.jpg")){
  ns <- session$ns
  
  output$racems_graph <- renderAmCharts({
    
    cur_am <- am_graph()
    
    if(!is.null(cur_am)){
      add_export <- F
      if(!"export" %in% names(attr(cur_am, "otherProperties"))){
        add_export <- T
      } else {
        if(!attr(cur_am, "otherProperties")$export$enabled){
          add_export <- T
        }
      }
      
      if(add_export){
        cur_am <- setExport(cur_am, enabled = TRUE, menu = list())
      }
      cur_am <- addListener(cur_am, name = "rendered", expression = paste0('function(e) {
                                                           // WAIT FOR FABRIC
                                                           var interval = setInterval( function() {
                                                           if ( window.fabric ) {
                                                           clearTimeout( interval );
                                                           // CAPTURE CHART
                                                           e.chart["export"].capture( {}, function() {
                                                           // SAVE TO JPG
                                                           this.toJPG( {}, function( base64 ) {
                                                           // LOG IMAGE DATA
                                                           Shiny.onInputChange("', ns("racems_img64"), '", base64);
                                                           // clean
                                                           this.setup.chart.div.style = "display:none";
                                                           e.chart.clear();
                                                           } );
                                                           } );
                                                           }
                                                           }, 100 );}'))
      cur_am
  } else {
    NULL
  }
    
    })
  
  # outputOptions(output,"am_graph", suspendWhenHidden = FALSE)
  
  path_file <- shiny::reactive({
    if(!is.null(name())){
      paste0(path(), "/", name())
    } else {
      NULL
    }
  })
  
  shiny::observe({
    if(!is.null(input$racems_img64)){
      if(input$racems_img64 != "" & !is.null(path_file()) & !is.null(am_graph())){
        outconn <- file(path_file(), "wb")
        base64enc::base64decode(what=gsub("^data:image/jpeg;base64,", "", input$racems_img64), output = outconn)
        close(outconn)
      }
    }
  })
  
  return(path_file)
  }