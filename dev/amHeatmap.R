#' Associeted colors to data.frame
#' @param data : data.frame
#' @param nbclasses : number of classes
#' @param col : 3 col to use in colorRampPalette
#' @param colorby : can be "all","row","col". 
#' 
#' 
#' @return data.frame compound to original data.frame and associated color data.frame
colorData <- function(data,nbclasses=NULL,col=c("#FF0000","#FFFFFF","#0000FF"),colorby="all")
{
  
  if(colorby=="all")
  {
    framclasses <- matrix(0,nrow=nrow(data),ncol=ncol(data))
    values <- unlist(c(data))
    if(nbclasses < length(unique(values))){
      classes <- quantile(values,seq(from = 0, to = 1,length.out = nbclasses+1))
      for(i in 1:(length(classes)-1))
      {
        framclasses=framclasses+((data>=classes[i])+1-1)
      }
    }else{
      nbclasses <- length(unique(values)) 
      classes <- sort(unique(values))
      for(i in 1:length(classes))
      {
        framclasses=framclasses+((data>=classes[i])+1-1)
      }
    }
  }
  
  if(colorby=="col")
  {
    framclasses <- matrix(0,nrow=nrow(data),ncol=ncol(data))
    for(j in 1:ncol(data))
    {
    classes <- quantile(sort((unlist(c(data[,j])))),seq(from = 0, to = 1,length.out = nbclasses+1))

    for(i in 1:(length(classes)-1))
    {
      framclasses[,j]=framclasses[,j]+((data[,j]>=classes[i])+1-1)
    }
    }
  }
  
  if(colorby=="row")
  {
    framclasses <- matrix(0,nrow=nrow(data),ncol=ncol(data))
    for(j in 1:nrow(data))
    {
    
    classes <- quantile(sort((unlist(c(data[j,])))),seq(from = 0, to = 1,length.out = nbclasses+1))
    for(i in 1:(length(classes)-1))
    {
      framclasses[j,]=framclasses[j,]+((data[j,]>=classes[i])+1-1)
    }
    }
  }

  color <- colorRampPalette(col)(nbclasses)
  for(i in 1:length(color)){
    framclasses[framclasses==as.character(i)] <- color[i]
  }
  framclasses <- data.frame(framclasses)
  names(framclasses) <- paste0(names(data),"col")
  framclasses[] <- lapply(framclasses, as.character)
  list(data = cbind(data,framclasses), classes = list(nclasses = nbclasses, labels = classes))
}

#' Associeted constructor data.frame to initial data.frame
#' @param data : data.frame
#' 
#' @return data.frame compound to original data.frame and associated constructor data.frame
constructdata <- function(data){
  construct <- matrix(1,ncol=ncol(data)/2,nrow=nrow(data))
  construct <- data.frame(construct)
  names(construct) <- paste0(names(data)[1:(ncol(data)/2)],"construct")
  return(cbind(row=row.names(data),data,construct))
}


#' Make chart
#' @param data : data.frame
#' @param labels : TRUE FALSE, display labels
#' @param cex : size of labels
#' @param xLabelsRotation : rotation of xlabels
#' @param colorby : can be "all","row","col". 
#' @param col : 3 col to use in colorRampPalette
#' @param nbclasses : number of classes
#' 
#' 
#' @return data.frame compound to original data.frame and associated constructor data.frame
heatmap <- function(data, classes, labels = TRUE,cex=10,main="",xLabelsRotation=45,colorby="all",col=c("#FF0000","#FFFFFF","#0000FF")){

  ncate <-(ncol(data)-1)/3
  
  namecat <- names(data[,2:(ncate+1)])
  
  values <- paste0("['", paste(namecat, collapse = "','"), "']")
  
  
  chart <- sapply(namecat,
                  
                  function(x){
                    
                    amGraph(balloonText=paste0("<b>[[title]]-[[category]]</b><br><b> count : </b>[[",x,"]]"),
                            fillAlphas=0.8,labelText=if(labels){paste0("[[",x,"]]")}else{""},lineAlpha=0.3,fontSize=cex,
                            title=x,type="column",fillColorsField=paste0(x,"col"),valueField=paste0(x,"construct"))},USE.NAMES = FALSE
  )

  guides = list()
  n <- length(colnames(data[,2:(ncate+1)]))
  k <- 0
  for(i in 1:n)
  {
    k <- k +1
    guides[[k]] <- guide(id=paste0("guide",i),value=i,toValue=i,lineAlpha=1,color="#000000",lineThickness=1)
    
  }
  n <- nrow(data)
  for(i in 1:n)
  {
    
    guides[[k]] = guide(id=paste0("guide",k),category=row.names(data)[i],lineAlpha=1,color="#000000",lineThickness=1,above=TRUE,expand=TRUE)
    k <- k +1
  }

  legendlist <- list()

  if(colorby=="all")
  {
    
    nbclasses <- classes$nclasses
    classes <- classes$labels
    color <- colorRampPalette(col)(nbclasses)
    
    associated <- NULL
    if(nbclasses < length(classes)){
      for(i in 1:length(classes)-1){
        associated[i] <- paste0("[",classes[i]," , ",classes[i+1], ifelse(i==length(classes)-1, "]", "["))
      }
    }else{
      associated <- classes
    }
    
    datatemp <- data.frame(title=associated,color=color)
    for(i in 1:nrow(datatemp))
    {
      legendlist[[i]] <- list(title=as.character(datatemp[i,1]),color = as.character(datatemp[i,2]))
    }
  }else{
    legendlist[[1]]<-list(title="Low",color = as.character(col)[1])
    legendlist[[2]]<-list(title="Medium",color = as.character(col)[2])
    legendlist[[3]]<-list(title="Large",color = as.character(col)[3])
  }
  
  
  amSerialChart()%>>%
    setBalloon(borderThickness = 0) %>>%
    setDataProvider(data) %>>%
    setProperties(type="serial",theme="light",columnWidth=1,categoryField="row",gridAboveGraphs=TRUE,rotate=TRUE)%>>%
    setGuides(guides)%>>%
    addTitle(text=main)%>>%
    setLegend(data=(legendlist),markerBorderColor="#000000", align = "center")%>>%
    addValueAxes(stackType="regular",axisAlpha=0,gridThickness=0,gridAlpha=1,position="left",labelRotation=xLabelsRotation,maximum=ncate,
      labelFunction = htmlwidgets::JS(paste0("function(value,valueString,axis){
        Math.trunc = Math.trunc || function(x) {
          return x < 0 ? Math.ceil(x) : Math.floor(x);
        };                                                                                                         
        var val = ", values, ";
        var indice = Math.trunc(value);
        if(indice < val.length && value % 1 != 0){
          return val[indice];
        }else{
          return '';
        }
      ;}")))%>>%
    setGraphs(chart)%>>%
    setCategoryAxis(gridPosition="start",axisAlpha=1,gridThickness=0,gridAlpha=1)%>>%
    setExport(enabled = TRUE, 
              menu = list(
                list(
                  class = "export-main",
                  menu = list(
                    list(
                      label = "Download as ...",
                      menu = list("PNG", "JPG", "SVG", "PDF")
                    ),
                    list(
                      label = "Save data as CSV",
                      click = htmlwidgets::JS(paste0('function() {

                        var cfg = {
				                  data: this.getChartData(),
                          delimiter: ",",
                          quotes: true,
                          escape: true,
                          dateFields: [],
                          dateFormat: this.setup.chart.dataDateFormat || "YYYY-MM-DD"
                        };

                        var data = "";
                                                     
                        if ( this.setup.chart.categoryAxis && this.setup.chart.categoryAxis.parseDates && this.setup.chart.categoryField ) {
                          cfg.dateFields.push( this.setup.chart.categoryField );
                        }
                        
                        //header
                        row = 0;
                        var buffer = [];
                        var cpt = 1;
                        for ( col in cfg.data[ row ] ) {
                        if(cpt <= ', ((ncol(data)-1)/3)+1, '){
                          var value = cfg.data[ row ][col];
                          value = col;
                                                    
                          if ( typeof value === "string" ) {
                            if ( cfg.escape ) {
                              value = value.replace( \'"\', \'""\' );
                            }
                            if ( cfg.quotes ) {
                              value = [ \'"\', value, \'"\' ].join( "" );
                            }
                          }
                                                     
                          buffer.push( value );
                          cpt = cpt+1;
                        }
                        }
                        data += buffer.join( cfg.delimiter ) + "\\n";
                                                      
                        for ( row in cfg.data ) {
                          var cpt = 1;
                          var buffer = [];
                          
                          for ( col in cfg.data[ row ] ) {
                            if(cpt <= ', ((ncol(data)-1)/3)+1, '){
                            var value = cfg.data[ row ][ col ];
                                                     
                            if ( typeof value === "string" ) {
                              value = value;
                            } else if ( cfg.dateFormat && value instanceof Date && cfg.dateFields.indexOf( col ) != -1 ) {
                              value = AmCharts.formatDate( value, cfg.dateFormat );
                            }
                            
                            // WRAP IN QUOTES
                            if ( typeof value === "string" ) {
                              if ( cfg.escape ) {
                                value = value.replace( \'"\', \'""\' );
                              }
                              if ( cfg.quotes ) {
                                value = [ \'"\', value, \'"\' ].join( "" );
                              }
                            }
                                                     
                            buffer.push( value );
                            cpt = cpt +1;
                          }
                          }
                          data += buffer.join( cfg.delimiter ) + "\\n";
                        };
                      this.download( data, "text/plain", "heatmap.csv" );}')
                    ))
                    )
                  )
                )
            
          ) %>>%
    plot
  
}


#' Amchart Heat-Map
#' @param data : data.frame, should be a contingency table
#' @param nbclasses : number of classes
#' @param col : 3 col to use in colorRampPalette
#' @param labels : TRUE FALSE, display labels
#' @param cex : size of labels
#' @param main : title
#' @param xLabelsRotation : rotation of xlabels
#' @param colorby : can be "all","row","col". 
#' @param legend : TRUE or FALSE, display legend
#' 
#' @examples
#' 
#' data(USArrests, "VADeaths")
#' USArrests <- USArrests [1:10,]
#' amheatmap(USArrests)
#' amheatmap(USArrests, nclasses=5, col=c("#FF0000","#FFFFFF","#0000FF"),labels = TRUE, cex=10,main="My title",xLabelsRotation=45,colorby="all",legend = TRUE)
#' amheatmap(USArrests, nclasses=5, col=c("#FF0000","#FFFFFF","#0000FF"),labels = TRUE, cex=10,main="My title",xLabelsRotation=45,colorby="row",legend = TRUE)
#' amheatmap(USArrests, nclasses=5, col=c("#FF0000","#FFFFFF","#0000FF"),labels = TRUE, cex=10,main="My title",xLabelsRotation=45,colorby="col",legend = TRUE)
#' amheatmap(USArrests, nclasses=10, col=c("#00FF00","#FF00FF","#0000FF"),labels = TRUE, cex=10,main="My title",xLabelsRotation=45,colorby="all",legend = TRUE)
#' @return data.frame compound to original data.frame and associated constructor data.frame
#' 
#' @export
amheatmap <- function(data, nclasses = 5, col = c("#FF0000","#FFFFFF","#0000FF"), labels = TRUE, cex=10, main="", 
                      xLabelsRotation=45, colorby="all", legend = TRUE){
  colordata <- colorData(data,nclasses,col,colorby)
  data <- constructdata(colordata$data)
  heatmap(data, colordata$classes, labels, cex, main, xLabelsRotation, colorby,col)
}

# data <- USArrests
# 
# data <- data.frame(a = c(3,0), b = c(2,1))
# amheatmap(data)
# 
# nclasses = 5
# col = c("#FF0000","#FFFFFF","#0000FF")
# colorby="all"

# toCharData <- function(data){
#   
#   res <- paste0(paste("'row'", paste0("'", paste(colnames(data), collapse = "','"), "'"), sep = ","), "\\n")
#   
#   ctrl <- sapply(1:nrow(data), function(x){
#     ligne <- paste0(paste(paste0("'", rownames(data)[x], "'"), paste0("'",paste(data[x, ], collapse = "','"), "'"), sep = ","), ifelse(x==nrow(data), "", "\\n"))
#     res <<- paste0(res, ligne)
#     NULL
#   })
#   
#   res
# }