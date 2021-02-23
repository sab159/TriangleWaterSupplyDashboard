/*########################################################################################################3
#
#
##########################################################################################################3*/
function createGWTab(gwID, recentDate) {
//parse date to scale axis
parseDate = d3.timeParse("%Y-%m-%d");

//read in stream stats
d3.csv("data/gw/gw_stats.csv").then(function(gwStats){
    gwStats.forEach(function(d){
            d.julian = +d.julian;
            d.min = +d.min;
            d.flow10 = +d.flow10;
            d.flow25 = +d.flow25;
            d.flow50 = +d.flow50;
            d.flow75 = +d.flow75;
            d.flow90 = +d.flow90;
            d.flow = +d.depth_below_surface_ft;
            d.max = +d.max;
            d.Nobs = +d.Nobs;
            d.start_yr = +d.start_yr;
            d.end_yr = +d.end_yr;
            d.date = parseDate(d.date2);
       });

var filterData = gwStats.filter(function(d){return d.site === gwID; });
var todayGW = filterData.filter(function(d){ return d.julian === recentDate; });
//console.log(filterData);

document.getElementById("selectGWMetadata").innerHTML = "Data from: " + filterData[0].start_yr + "-" + 
         filterData[0].end_yr + " (~" + d3.median(filterData, function(d) { return d.Nobs; }) + " years with observations) <br><span style='color: rgb(26,131,130);'>The last measurement was taken on " + 
         filterData[0].date.toLocaleDateString("en-US") + ".</span><br><br>";
});//end D3

//#####################################################################################
//         PLOT BY STATUS AS MARKERS
//#####################################################################################//

//read in stream stats
d3.csv("data/gw/gw_levels_time.csv").then(function(gwLevels){
    gwLevels.forEach(function(d){
            d.julian = +d.julian;
            d.flow = +d.depth_below_surface_ft;
            d.date = parseDate(d.date);
       });
  //console.log(gwLevels)
  var selGWLevels = gwLevels.filter(function(d){return d.site === gwID; });
  var xDate = selGWLevels.map(function(d) {return d.date; });
  var yDepth = selGWLevels.map(function(d) {return d.flow; });
  var colorPoints = selGWLevels.map(function(d) {return d.colorStatus; });
  var streamStatus = selGWLevels.map(function(d) {return d.status; });
  var data2;
  var minVal = Math.max(yDepth);

  traceColorStatus = {
    type: 'scatter',
    x: xDate,  y: yDepth,
    text: streamStatus,
    mode: 'lines+markers',
    name: '',
    marker: { color: colorPoints, size: 5, opacity: 0.8},
    line: { color: 'gray',  width: 1},
    hovertemplate:
              "<b>%{text}</b><br>" +
              "Depth (ft): %{y:.2f}<br>" +
              "Date: %{x}"
  };
  var gwlayout2 = {
    yaxis: {
        title: "Feet below surface (ft)",
        titlefont: {color: 'rgb(0, 0, 0)', size: 14},
        tickfont: {color: 'rgb(0, 0, 0)', size: 12},
        showline: true,
        showgrid: false,
        range: [0, minVal],
        autorange: "reversed"
    },
    xaxis: {
      showline: true,
      title: '',
      titlefont: {color: 'rgb(0, 0, 0)', size: 14},
      tickfont: {color: 'rgb(0, 0, 0)', size: 12},
      tickformat: '%b-%Y',
    },
    height: 300,
    showlegend: false,
    margin: {t: 30, b: 60, r: 30, l: 50 },
    shapes: [{
      type: 'line', xref: 'paper', yref: 'y',
      x0: 0, x1: 1, y0:0, y1: 0,
      line: {color: "#745508", width: "2"}
    }],
    annotations: [
        //above ground
          { xref: 'paper', yref: 'paper', //ref is assigned to x values
            x: 0.6, y: 1,
            xanchor: 'left', yanchor: 'top',
            text: "Land Surface", 
            font: {family: 'verdana', size: 11, color: '#745508'},
            showarrow: false,
          }
    ]
};
data2 = [traceColorStatus];

//console.log(data);
Plotly.newPlot('gwPlot2', data2, gwlayout2);
});//end DE


//#####################################################################################
//         PLOT BY STATUS AS MARKERS
//#####################################################################################//

//read in stream stats
d3.csv("data/gw/gw_annual_level.csv").then(function(gwAnnual){
    gwAnnual.forEach(function(d){
            d.flow = +d.medianDepth;
            d.year = +d.year;
       });

  var selGWAnnual = gwAnnual.filter(function(d){return d.site === gwID; });
  var xYear = selGWAnnual.map(function(d) {return d.year; });
  var yDepth = selGWAnnual.map(function(d) {return d.flow; });
  var minVal = Math.max(yDepth);

  traceAnnual = {
    type: 'scatter',
    x: xYear,  y: yDepth,
    text: "median Depth",
    mode: 'lines+markers',
    name: '',
    marker: { color: "black", size: 5, opacity: 0.8},
    line: { color: 'gray',  width: 1},
    hovertemplate:
              "Median Depth (ft): %{y:.2f} in %{x}"
  };
  var gwlayout3 = {
    yaxis: {
        title: "Feet below surface (ft)",
        titlefont: {color: 'rgb(0, 0, 0)', size: 14},
        tickfont: {color: 'rgb(0, 0, 0)', size: 12},
        showline: true,
        showgrid: false,
        range: [0, minVal],
        autorange: "reversed"
    },
    xaxis: {
      showline: true,
      title: '',
      titlefont: {color: 'rgb(0, 0, 0)', size: 14},
      tickfont: {color: 'rgb(0, 0, 0)', size: 12},
    },
    height: 300,
    showlegend: false,
    margin: {t: 30, b: 60, r: 30, l: 50 },
    shapes: [{
      type: 'line', xref: 'paper', yref: 'y',
      x0: 0, x1: 1, y0:0, y1: 0,
      line: {color: "#745508", width: "2"}
    }],
    annotations: [
        //above ground
          { xref: 'paper', yref: 'paper', //ref is assigned to x values
            x: 0.6, y: 1,
            xanchor: 'left', yanchor: 'top',
            text: "Land Surface", 
            font: {family: 'verdana', size: 11, color: '#745508'},
            showarrow: false,
          }
    ]
};
var data3 = [traceAnnual];
Plotly.newPlot('gwPlot3', data3, gwlayout3);
});//end DE

} //END CREATE CHART FUNCTION ##########################################################

