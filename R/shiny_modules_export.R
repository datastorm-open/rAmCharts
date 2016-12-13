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
#' @param path character, directory. tempdir() by Defaut
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
  shiny::uiOutput(ns("racems_graphs_ui"))
}


#' @rdname rAmCharts-shinymodules
#' @export
rAmChartExportServer <- function(input, output, session, list_am_graph, 
                                 path = shiny::reactive(tempdir())){
  ns <- session$ns
  
  cpt <- shiny::reactiveValues(ind = -1, max = -1)
  
  shiny::observe({
    list_chart <- list_am_graph()
    if(!is.null(list_chart)){
      cpt$ind = 1
      cpt$max = length(list_chart)
    } else {
      cpt$ind = -1
    }
  })
  
  output$racems_graphs_ui <- shiny::renderUI({
    ind_chart <- cpt$ind
    if(ind_chart != -1 & ind_chart <= cpt$max){
      
      shiny::isolate({
        # chart
        tmp_chart <- list_am_graph()[[cpt$ind]]
        
        # ui
        args_amco <- formals(amChartsOutput)
        
        if("width" %in% names(tmp_chart)){
          width <- tmp_chart$width
        } else {
          width <- args_amco$width
        }
        
        if("height" %in% names(tmp_chart)){
          height <- tmp_chart$height
        } else {
          height <- args_amco$height
        }
        
        if("type" %in% names(tmp_chart)){
          type <- tmp_chart$type
        } else {
          type <- args_amco$type
        }
        
        amChartsOutput(ns("racems_graph"), 
                       width = width, height = height, type = type)
      })
    }
  })
  
  
  output$racems_graph <- renderAmCharts({
    ind_chart <- cpt$ind
    if(ind_chart != -1 & ind_chart <= cpt$max){
      shiny::isolate({
        cur_am <- list_am_graph()[[ind_chart]]$graph
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
                                                                               //e.chart.events.rendered = [];
                                                                               //e.chart.clear();
                                                                               //this.setup.chart.div.style = "display:none";
                                                                               } );
                                                                               } );
                                                                               }
                                                                               }, 100 );}'))
          cur_am
        } else {
          NULL
        }
      })
    }
  })
  
  shiny::observe({
    if(!is.null(input$racems_img64)){
      shiny::isolate({
        if(input$racems_img64 != ""){
          path_file <- paste0(path(), "/", list_am_graph()[[cpt$ind]]$name)
          outconn <- file(path_file, "wb")
          base64enc::base64decode(what=gsub("^data:image/jpeg;base64,", "", input$racems_img64), output = outconn)
          close(outconn)
          cpt$ind <- cpt$ind +1
        }
      })
      
    }
  })
  
  return(path)
}