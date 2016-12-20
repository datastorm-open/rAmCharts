HTMLWidgets.widget({

  name: 'amDrillChart',

  type: 'output',

  // for instance initialization
  initialize: function (el, width, height) {
    return {};
  },

  test: function () {
    var a = 2;
  },

  renderValue: function (el, data, instance) {

    // clear el.id (for shiny... ?)
    document.getElementById(el.id).innerHTML = "";

    // go back button
    var backbutton = document.createElement("button");
    backbutton.id = "backbtn" + el.id;
    var t = document.createTextNode("<< BACK");       // Create a text node
    backbutton.appendChild(t);
    backbutton.setAttribute('style', 'visibility:hidden; background-color:transparent; color:#3c8dbc; border: none');
    backbutton.onclick = function resetChart() {
      //restore features
      if (instance.chart) {
        for (var key in instance.main) {
          instance.chart[key] = instance.main[key];
        }

        //display firstGraph
        instance.chart.validateData();
        instance.chart.animateAgain();

        // hide button
        document.getElementById("backbtn" + el.id).style.visibility = "hidden";
      }
    };
    document.getElementById(el.id).appendChild(backbutton);

    // div for graph
    var graph = document.createElement('div');
    graph.id = "graph" + el.id;
    graph.style.width = "100%";
    graph.style.height = "95%";
    graph.style.background = data.background;
    document.getElementById(el.id).appendChild(graph);

    // graph
    instance.main = data.main;
    instance.chart = AmCharts.makeChart(document.getElementById(graph.id), data.main);
    for (var key in data.listeners) {
      instance.chart.addListener(key, data.listeners[key]);
    }
    // Ajout du listener
    if (data.main.type === "serial" || data.main.type === "radar") {
      // alert("serial or pie") ok !
      if (instance.chart) {
        addSerialRadarListener(instance.chart, data.subProperties);
      } else { }
    } else if (data.main.type === "pie") {
      /*
      if(instance.chart){
        addPieListener(instance.chart, data.subProperties) ;
      }else{}
      
      var chart;
      var legend;
      var selected;
      var types = data.main.dataProvider ;
    
    function addPieListener(chart, subProperties) {
      chart.addListener('clickSlice', function (event){
        //console.info(event)
        if (event.dataItem.dataContext.id != undefined) {
              selected = event.dataItem.dataContext.id;
        } else {
              selected = undefined;
        }
        event.char.dataProvider = generateChartData();
        event.chart.validateData() ;
      }
    });
    
    function generateChartData () {
        var chartData = [];
        for (var i = 0; i < types.length; i++) {
          if (i == selected) {
            for (var x = 0; x < types[i].subs.length; x++) {
                chartData.push({
                  key: types[i].subs[x].key,
                  value: types[i].subs[x].value,
                  color: types[i].color,
                  pulled: true
                });
            }
          }
          else {
            chartData.push({
                key: types[i].key,
                value: types[i].value,
                color: types[i].color,
                id: i
            });
          }
        }
      return chartData;
    }
    */
    } else { }

    function addSerialRadarListener(chart, subProperties) {
      // alert("ajout d'un listener") ok !
      chart.addListener('clickGraphItem', function (event) {
        //console.info(event)
        if (event.item.dataContext.subdata) {
          // set the utility coefficients for the clicked
          event.chart.dataProvider = event.item.dataContext.subdata;

          for (var key in subProperties) {
            // a tester : reset event.chart et tout recreer ?
            // donc il faudrait passer toutes les proprietes
            delete event.chart[key];
            event.chart[key] = subProperties[key];
          }

          // validate the new data and make the chart animate again
          event.chart.validateData();
          event.chart.animateAgain();

          // show button
          document.getElementById(backbutton.id).style.visibility = "visible";
        }
      });
    }
  }
});