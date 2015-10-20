HTMLWidgets.widget({
  
  name: 'ramcharts_base',
  
  type: 'output',
  
  initialize: function(el, width, height) {
    return {};
  },
  
  resize: function(el, width, height, instance) {
    if (instance.amchart) {
      instance.amchart.resize();
    } else {}
  },
  
  renderValue: function(el, x, instance) {
    var chartDiv = document.getElementById(el.id);
    chartDiv.style.background = x.background;
    chartDiv.style.fontSize = '11px';
    instance.amchart = AmCharts.makeChart(chartDiv, x.chartData);
    
    // add chart listeners
    for (var key in x.listeners) {
      instance.amchart.addListener(key, x.listeners[key]);
    }
    
    if (window.Shiny) {
      handleInit();
    } else {
      instance.amchart.addListener("init", handleInit);
    }
    
    function handleInit() {
      var key_handle;
      var indice;
      
      for (indice in x.axes_listenersIndices) {
        // JavaScript reduces indices by 1
        // so, no need to do indice = indice -1
        for (key_handle in x.axes_listeners[indice]) {
          instance.amchart.axes[indice].addListener(key_handle, x.axes_listeners[indice][key_handle]);
        }
      }
      
      for (key_handle in x.categoryAxis_listeners) {
        instance.amchart.categoryAxis.addListener(key_handle, x.categoryAxis_listeners[key_handle]);
      }
      
      for (key_handle in x.chartCursor_listeners) {
        instance.amchart.chartCursor.addListener(key_handle, x.chartCursor_listeners[key_handle]);
      }
      
      for (key_handle in x.dataSetSelector_listeners) {
        instance.amchart.dataSetSelector.addListener(key_handle, x.dataSetSelector_listeners[key_handle]);
      }
      
      for (key_handle in x.legend_listeners) {
        instance.amchart.legend.addListener(key_handle, x.legend_listeners[key_handle]);
      }
      
      for (indice in x.panels_listenersIndices) {
        // JavaScript reduces indices by 1
        // so, no need to do indice = indice -1
        for (key_handle in x.panels_listeners[indice]) {
          instance.amchart.panels[indice].addListener(key_handle, x.panels_listeners[indice][key_handle]);
        }
      }
      
      for (key_handle in x.periodSelector_listeners) {
        instance.amchart.periodSelector.addListener(key_handle, x.periodSelector_listeners[key_handle]);
      }
      
      for (indice in x.valueAxes_listenersIndices) {
        // JavaScript reduces indices by 1
        // so, no need to do indice = indice -1
        for (key_handle in x.valueAxes_listeners[indice]) {
          instance.amchart.valueAxes[indice].addListener(key_handle, x.valueAxes_listeners[indice][key_handle]);
        }
      }
    }
    
  }
  
});

