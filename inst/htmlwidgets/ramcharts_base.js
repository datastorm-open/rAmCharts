HTMLWidgets.widget({
  
  name: 'ramcharts_base',
  
  type: 'output',
  
  initialize: function(el, width, height) {
    return {};
  },
  
  renderValue: function(el, x, instance) {
    var chartDiv = document.getElementById(el.id);
    chartDiv.style.background = x.background;
    chartDiv.style.fontSize = '11px';
    instance.amchart = AmCharts.makeChart(chartDiv, x.chartData);
    
    // add listeners for chart listeners
    if (x.listeners) {
      for (var key in x.listeners) {
        instance.amchart.addListener(key, x.listeners[key]);
      }
    }
    
    // Add legend listeners
    // the chart must be initialized before
    if(x.legend_listeners !== undefined) {
      if (window.Shiny) {
        for (var key2 in x.legend_listeners) {
          instance.amchart.legend.addListener(key2, x.legend_listeners[key2]);
        }
      } else {
        instance.amchart.addListener("init", handleInit);
      }
    }
    
    
    function handleInit() {
      for (var key in x.legend_listeners) {
        instance.amchart.legend.addListener(key, x.legend_listeners[key]);
      }
    }
    
  },
  
  resize: function(el, width, height, instance) {}
  
});
