HTMLWidgets.widget({
  
  name: 'amSerialChart',
  
  type: 'output',
  
  initialize: function(el, width, height) {
    return {};
  },
  
  renderValue: function(el, x, instance) {
    var chartDiv = document.getElementById(el.id);
    chartDiv.style.background = x.background;
    instance.amchart = AmCharts.makeChart(chartDiv, x.chartData);
  
    for (var key in x.chartData.listeners) {
      instance.amchart.addListener(key, x.chartData.listeners[key]);
    }
    
    // Add listeners for legend
    // the chart must be initialized before
    instance.amchart.addListener("init", handleInit);
    function handleInit() {
      for (var key in x.chartData.legend.listeners) {
        instance.amchart.legend.addListener(key, x.chartData.legend.listeners[key]);
      }
    }
  },
  
  resize: function(el, width, height, instance) {}
  
});
