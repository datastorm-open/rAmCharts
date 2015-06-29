HTMLWidgets.widget({

  name: 'amFunnelChart',

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
    
    //document.getElementById(el.id).className = "responsive-container";
    //document.getElementById('htmlwidget_container').className = "container";
  },
  
  resize: function(el, width, height, instance) {
    instance.amchart.invalidateSize();
  }

});
