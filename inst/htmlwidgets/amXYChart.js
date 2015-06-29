HTMLWidgets.widget({
  
  name: 'amXYChart',
  
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
  },
  
  resize: function(el, width, height, instance) {}
  
});
