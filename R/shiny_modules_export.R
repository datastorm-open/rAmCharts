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
#' @param progress boolean, set a progress bar or not ?
#' @param message charcter, if progress, message. Defaut to "Calculation in progress" 
#' @param detail charcter, if progress, detail. Defaut to "This may take a while...'
#' 
#' @return a reactive expression
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
#'        list(graph = amPie(data = data_pie), name = "pie.jpg", height = "200px", width = "300px"),
#'        list(graph = amBarplot(x = "country", y = "visits", data = data_bar, main = "example") %>%
#'          setExport(), name = "bar.jpg", height = "600px")
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
                                 path = shiny::reactive(tempdir()), progress = T, 
                                 message = 'Calculation in progress', 
                                 detail = 'This may take a while...'){
  ns <- session$ns
  
  # init progress if needeed
  progress_bar <- reactive({
    list_chart <- list_am_graph()
    if(!is.null(list_chart)){
      if(progress){
        progress <- Progress$new(session, min=0, max=length(list_chart))
        progress$set(message = message,
                     detail = detail, value = 0)
        progress
      } else {
        NULL
      }
    }else{
      NULL
    }
  })
  
  # some controls for rendering in order and for stock issue
  cpt <- shiny::reactiveValues(ind = -1, max = -1, tmp = 0)
  
  shiny::observe({
    list_chart <- list_am_graph()
    if(!is.null(list_chart)){
      cpt$ind <- 1
      cpt$max <- length(list_chart)
      cpt$tmp <- shiny::isolate(cpt$tmp) + 1
    } else {
      cpt$ind <- -1
      cpt$max <- -1
    }
  })
  
  # for conditionnalPanel
  output$racems_n_g_evo <- shiny::reactive({
    cpt$ind
  })
  
  shiny::outputOptions(output, "racems_n_g_evo", suspendWhenHidden = FALSE)
  
  # ui output
  output$racems_graphs_ui <- shiny::renderUI({
    tmp_cpt <- cpt$tmp
    if(tmp_cpt > 0){
      shiny::isolate({
        list_chart <- list_am_graph()
        args_amco <- formals(amChartsOutput)
        
        if(length(list_chart) > 0){
          plot_output_list <- lapply(1:length(list_chart), function(i) {
            plotname <- ns(paste0("racems_graph_", i, "_", shiny::isolate(cpt$tmp)))
            
            tmp_chart <- list_chart[[i]]
            
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
            
            shiny::conditionalPanel(condition = paste0("output['", ns("racems_n_g_evo"), "'] == ", i),
                                    shiny::div(amChartsOutput(plotname, width = width, height = height, type = type), br(), align = "center")
            )
          })
          shiny::fluidRow(
            do.call(shiny::tagList, plot_output_list)
          )
        }
      })
    }
  })
  
  shiny::observe({
    ind_chart <- cpt$ind
    if(ind_chart != -1 & ind_chart <= cpt$max){
      shiny::isolate({
        plotname <- paste0("racems_graph_", ind_chart, "_", shiny::isolate(cpt$tmp))
        output[[plotname]] <- renderAmCharts({
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
          
          # increase/close progress
          progress_bar <- progress_bar()
          if(!is.null(progress_bar)){
            progress_bar$set(cpt$ind)
            if(cpt$ind == cpt$max){
              progress_bar$close()
            }
          }
          cpt$ind <- cpt$ind +1
        }
      })
      
    }
  })
  
  info <- reactive({
    cur_g <- cpt$ind
    max_g <- cpt$max
    if(cur_g > max_g){
      cur_g <- -1
      max_g <- -1
    }
    list(current_graph = cur_g, max_graph = max_g, dir = path())
  })
  
  return(info)
}