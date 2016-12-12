// function to get amChart chart with div id
function getAmChart(id) {
    var allCharts = AmCharts.charts;
    if(allCharts !== undefined){
        for (var i = (allCharts.length - 1); i > -1; i--) {
            if(allCharts[i].div !== undefined){ // for markdown bug ?
                if (id == allCharts[i].div.id) {
                    return allCharts[i];
                }
            }
        }
    }
}

var amStock_ref_group = {};

function addAmStockRefGroup(group, id){
  if (amStock_ref_group.hasOwnProperty(group)) {
    if(amStock_ref_group[group].indexOf(id) === -1){
          amStock_ref_group[group].push(id);
    }
  }else {
    amStock_ref_group[group]= [id];
  }
}

function findAmStockRefGroup(id){
  for (var gr in amStock_ref_group) {
    if(amStock_ref_group[gr].indexOf(id) !== -1){
      return amStock_ref_group[gr];
    }
  }
  return undefined;
}

HTMLWidgets.widget({

    name: 'ramcharts_base',

    type: 'output',

    factory: function(el, width, height) {

        // add little processDelay
        AmCharts.processDelay = 10;

        // clear previous amChart graph in this div if existed
        var existing_chart = getAmChart(el.id);
        if (existing_chart) existing_chart.clear();

        // init the chart element, empty since we don't have any chart data
        var amchart;

        return {
            renderValue: function (x) {

                // set the background
                document.getElementById(el.id).style.background = x.background;
                amchart = AmCharts.makeChart(el.id, x.chartData);

                if(x.group !== null){
                    addAmStockRefGroup(x.group, el.id);
                    amchart.panels[0].addListener("zoomed", function(event){
                      var linked_chart = findAmStockRefGroup(el.id);
                      if(linked_chart !== undefined){
                        for(var tmp_id in linked_chart){
                          if(linked_chart[tmp_id] !== el.id){
                            var tmp_am = getAmChart(linked_chart[tmp_id]);
                              if(tmp_am !== undefined){
                                tmp_am.panels[0].zoomToDates(event.startDate, event.endDate);
                              }
                          }   
                        }
                      }
                    });
                }
                
                
                // add chart listeners
                for (var key in x.listeners) {
                    if (x.listeners.hasOwnProperty(key)) {
                        amchart.addListener(key, x.listeners[key]);
                    }
                }

                if (window.Shiny) {
                    handleInit();
                } else {
                    amchart.addListener("init", handleInit);
                }


                function handleInit() {
                    var key_handle;
                    var indice;

                    for (indice in x.axes_listenersIndices) {
                        // JavaScript reduces indices by 1
                        // so, no need to do indice = indice -1
                        for (key_handle in x.axes_listeners[indice]) {
                            amchart.axes[indice].addListener(key_handle, x.axes_listeners[indice][key_handle]);
                        }
                    }

                    for (key_handle in x.categoryAxis_listeners) {
                        amchart.categoryAxis.addListener(key_handle, x.categoryAxis_listeners[key_handle]);
                    }

                    for (key_handle in x.chartCursor_listeners) {
                        amchart.chartCursor.addListener(key_handle, x.chartCursor_listeners[key_handle]);
                    }

                    for (key_handle in x.dataSetSelector_listeners) {
                        amchart.dataSetSelector.addListener(key_handle, x.dataSetSelector_listeners[key_handle]);
                    }

                    for (key_handle in x.legend_listeners) {
                        amchart.legend.addListener(key_handle, x.legend_listeners[key_handle]);
                    }

                    for (indice in x.panels_listenersIndices) {
                        // JavaScript reduces indices by 1
                        // so, no need to do indice = indice -1
                        for (key_handle in x.panels_listeners[indice]) {
                            amchart.panels[indice].addListener(key_handle, x.panels_listeners[indice][key_handle]);
                        }
                    }

                    for (key_handle in x.periodSelector_listeners) {
                        amchart.periodSelector.addListener(key_handle, x.periodSelector_listeners[key_handle]);
                    }

                    for (indice in x.valueAxes_listenersIndices) {
                        // JavaScript reduces indices by 1
                        // so, no need to do indice = indice -1
                        for (key_handle in x.valueAxes_listeners[indice]) {
                            amchart.valueAxes[indice].addListener(key_handle, x.valueAxes_listeners[indice][key_handle]);
                        }
                    }
                }

                if (window.Shiny) {
                    var myevent;
                    if (amchart.events.init[0] !== undefined) {
                        myevent = {type: "init", chart: amchart};
                        amchart.events.init[0].handler(myevent);
                    }
                    if (amchart.events.rendered[0] !== undefined) {
                        myevent = {type: "rendered", chart: amchart};
                        amchart.events.rendered[0].handler(myevent);
                    }
                }
            },


            resize: function (width, height) {
                if (amchart) amchart.handleResize();
            },

            // Make the sigma object available as a property on the widget
            // instance we're returning from factory(). This is generally a
            // good idea for extensibility--it helps users of this widget
            // interact directly with sigma, if needed.
            chart: amchart
        };
    }
});

