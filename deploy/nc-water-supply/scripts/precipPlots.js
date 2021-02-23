function createTracePCP(target){
  checked = [];
  $("input[name='checkPCPYear']:checked").each(function() {
    checked.push($(this).val());
  });
  
  Plotly.purge('pcpMonthPlot'); Plotly.purge("pcpCumPlot");
  plotPrecipitation(pcpID, checked);
  return checked;
}

function plotPrecipitation(pcpID, checked){
//parse date to scale axis
parseDate = d3.timeParse("%Y-%m-%d");

d3.csv("data/pcp/triangle_pcp_months.csv").then(function(dfpcp){
  dfpcp.forEach(function(d){
    d.month = +d.month;           
    d.year = +d.year;
    d.pcp_in = +d.pcp_in;
  }); //end for each
  var selpcp = dfpcp.filter(function(d){ return d.id === pcpID; });

  //create a trace for each year
  var data = [];
  var xMonth = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
  var yOther; var OtherYearTrace;
  //draw the traces for all years but current
  var xYear = [];
  var minYear = d3.min(selpcp.map(function(d){return d.year; })); 
    
  for(var i = minYear; i <= 2020; i++){   xYear.push(i); }
  
  for (i=0; i < xYear.length-1; i++){
    tempSelect = xYear[i];
    temp = selpcp.filter(function(d) {return d.year === tempSelect; });
    tempName = "%{y:.1f} inches in %{x}, " + tempSelect;
    yOther = temp.map(function(d){ return d.pcp_in; });
    //create individual trace
      OtherYearTrace = 
          {
            x: xMonth,   y: yOther,
            mode: 'lines', type: 'scatter',
            hovertemplate: tempName,
            opacity: 0.4,
            line: {color: '#c5c5c5', width: 1}, //light coral
            name: ""
          };
    //push trace
    data.push(OtherYearTrace);
  } // end for loop

  //draw other selected years
  var selectYears;  var selectTraces; var colorLine;
  for (i=0; i < checked.length; i++){
    tempSelect = Number(checked[i]);
    selectYears = selpcp.filter(function(d) {return d.year === tempSelect;}).map(function(d) { return d.pcp_in; });
    tempName = "%{y:.1f} inches in %{x}, " + tempSelect;
    colorLine = '#625f67';
      if (tempSelect <= 2008) { colorLine = "red"; } // highlight drought years
      if (tempSelect === 2003 || tempSelect === 2018){ colorLine = "blue"; } // highlight wettest years on record

    selectTraces = {
            x: xMonth,   y: selectYears,
            mode: 'lines', type: 'scatter',
            hovertemplate: "%{y:.1f} inches in %{x}, " + tempSelect,
            opacity: 1,
            line: {color: colorLine, width: 3}, 
            name: ""
          };
    data.push(selectTraces);
  }
  
  //draw 2020 year
  var yCurrent = selpcp.filter(function(d) {return d.year === currentYear;}).map(function(d) { return d.pcp_in; });
  var trace2020 = {
            x: xMonth,   y: yCurrent,
            mode: 'lines', type: 'scatter',
            hovertemplate: "%{y:.1f} inches in %{x}, " + currentYear,
            opacity: 1,
            line: {color: 'black', width: 4}, 
            name: ""
          };
  data.push(trace2020);

 
 //PLOT CHART
 var layout = {
    yaxis: {
        title: 'Monthly Precip (Inches)',
        titlefont: {color: 'rgb(0, 0, 0)', size: 14 },
        tickfont: {color: 'rgb(0, 0, 0)', size: 12},
        showline: false,   showgrid: false,
        showticklabels: true,
        range: [0, 14]
    },
    xaxis: {
      showline: false,      showgrid: false,
      showticklabels: true,
      title: '',
      titlefont: {color: 'rgb(0, 0, 0)', size: 14},
      tickfont: {color: 'rgb(0, 0, 0)', size: 12},
    },
    hovermode: 'closest',
    height: 225,
    showlegend: false,
    margin: { t: 30,   b: 30,  r: 40,   l: 40  },
  };
  Plotly.newPlot('pcpMonthPlot', data, layout);
}); //end d3
//##################################################################################################################################
//                     END THAT PLOT
//##################################################################################################################################

//parse date to scale axis
//parseDate = d3.timeParse("%Y-%m-%d");
d3.csv("data/pcp/triangle_pcp_cumulative_yr.csv").then(function(cumpcp){
  cumpcp.forEach(function(d){
    //d.date = parseDate(d.date);
    d.year = +d.year;
    d.julian = +d.julian;
    d.pcp_in = +d.pcp_in;
  }); //end for each

  var selpcp = cumpcp.filter(function(d){ return d.id === pcpID; });

  //create a trace for each year
  var cumdata = [];
  var xJulian = [];
    for(var i = 0; i<= 365; i++){ xJulian.push(i); }
  var yOther; var OtherYearTrace;
  //draw the traces for all years but current
  var xYear = [];
  var minYear = d3.min(selpcp.map(function(d){return d.year; })); 
    
  for(var i = minYear; i <= 2020; i++){   xYear.push(i); }
  
  for (i=0; i < xYear.length-1; i++){
    tempSelect = xYear[i];
    temp = selpcp.filter(function(d) {return d.year === tempSelect; });
    tempName = "%{y:.1f} inches by %{x} days of " + tempSelect;
    //xJulian = temp.map(function(d) {return d.date; });
    yOther = temp.map(function(d){ return d.pcp_in; });
    //create individual trace
      OtherYearTrace = 
          {
            x: xJulian,   y: yOther,
            mode: 'lines', type: 'scatter',
            hovertemplate: tempName,
            opacity: 0.4,
            line: {color: '#c5c5c5', width: 1}, //light coral
            name: ""
          };
    //push trace
    cumdata.push(OtherYearTrace);
  } // end for loop

  //draw other selected years
  var selectYears;  var selectTraces; var colorLine;
  for (i=0; i < checked.length; i++){
    tempSelect = Number(checked[i]);
    selectYears = selpcp.filter(function(d) {return d.year === tempSelect;}).map(function(d) { return d.pcp_in; });
    tempName = "%{y:.1f} inches by %{x} day of " + tempSelect;
    colorLine = '#625f67';
      if (tempSelect <= 2008) { colorLine = "red"; } // highlight drought years
      if (tempSelect === 2003 || tempSelect === 2018){ colorLine = "blue"; } // highlight wettest years on record

    selectTraces = {
            x: xJulian,   y: selectYears,
            mode: 'lines', type: 'scatter',
            hovertemplate: "%{y:.1f} inches by %{x} days of " + tempSelect,
            opacity: 1,
            line: {color: colorLine, width: 3}, 
            name: ""
          };
    cumdata.push(selectTraces);
  }
  
  //draw 2020 year
  var yCurrent = selpcp.filter(function(d) {return d.year === currentYear;}).map(function(d) { return d.pcp_in; });
  var trace2020 = {
            x: xJulian,   y: yCurrent,
            mode: 'lines', type: 'scatter',
            hovertemplate: "%{y:.1f} inches by %{x} days of 2020",
            opacity: 1,
            line: {color: 'black', width: 4}, 
            name: ""
          };
  cumdata.push(trace2020);

 
 //PLOT CHART
 var layout = {
    yaxis: {
        title: 'Cumulative Precip (In)',
        titlefont: {color: 'rgb(0, 0, 0)', size: 14 },
        tickfont: {color: 'rgb(0, 0, 0)', size: 12},
        showline: false,   showgrid: false,
        showticklabels: true,
        range: [0, 76]
    },
    xaxis: {
      showline: false,      showgrid: false,
      showticklabels: true,
      title: '',
      titlefont: {color: 'rgb(0, 0, 0)', size: 14},
      tickfont: {color: 'rgb(0, 0, 0)', size: 12},
    },
    hovermode: 'closest',
    height: 225,
    showlegend: false,
    margin: { t: 30,   b: 30,  r: 40,   l: 40  },

    shapes: [
      {type: 'line',
        x0: 31,  y0: 0, x1: 31, y1: 1,  yref: 'paper', layer: 'below',
        line: {  color: 'grey',  width: 1,   dash: 'dot'   }
      },
      {type: 'line',
        x0: 59,  y0: 0, x1: 60, y1: 1,  yref: 'paper', layer: 'below',
        line: {  color: 'grey',  width: 1,   dash: 'dot'   }
      }, 
      {type: 'line',
        x0: 90,  y0: 0, x1: 91, y1: 1,  yref: 'paper', layer: 'below',
        line: {  color: 'grey',  width: 1,   dash: 'dot'   }
      }, 
      {type: 'line',
        x0: 120,  y0: 0, x1: 121, y1: 1,  yref: 'paper', layer: 'below',
        line: {  color: 'grey',  width: 1,   dash: 'dot'   }
      }, 
      {type: 'line',
        x0: 151,  y0: 0, x1: 152, y1: 1,  yref: 'paper', layer: 'below',
        line: {  color: 'grey',  width: 1,   dash: 'dot'   }
      }, 
      {type: 'line',
        x0: 181,  y0: 0, x1: 182, y1: 1,  yref: 'paper', layer: 'below',
        line: {  color: 'grey',  width: 1,   dash: 'dot'   }
      }, 
      {type: 'line',
        x0: 213,  y0: 0, x1: 213, y1: 1,  yref: 'paper', layer: 'below',
        line: {  color: 'grey',  width: 1,   dash: 'dot'   }
      }, 
      {type: 'line',
        x0: 244,  y0: 0, x1: 244, y1: 1,  yref: 'paper', layer: 'below',
        line: {  color: 'grey',  width: 1,   dash: 'dot'   }
      }, 
      {type: 'line',
        x0: 274,  y0: 0, x1: 274, y1: 1,  yref: 'paper', layer: 'below',
        line: {  color: 'grey',  width: 1,   dash: 'dot'   }
      }, 
      {type: 'line',
        x0: 305,  y0: 0, x1: 305, y1: 1,  yref: 'paper', layer: 'below',
        line: {  color: 'grey',  width: 1,   dash: 'dot'   }
      }, 
      {type: 'line',
        x0: 335,  y0: 0, x1: 335, y1: 1,  yref: 'paper', layer: 'below',
        line: {  color: 'grey',  width: 1,   dash: 'dot'   }
      }
    ],

    annotations:[
    { xref: 'x',  yref: 'paper',   x: 1,  y: 1,
      xanchor: 'left',   yanchor: 'top',  text: 'Jan',  showarrow: false, font: {size:10}
    },
    { xref: 'x',  yref: 'paper',   x: 33,  y: 1,
      xanchor: 'left',   yanchor: 'top',  text: 'Feb',  showarrow: false, font: {size:10}
    },
    { xref: 'x',  yref: 'paper',   x: 61,  y: 1,
      xanchor: 'left',   yanchor: 'top',   text: 'Mar',  showarrow: false, font: {size:10}
    },
    { xref: 'x',  yref: 'paper',   x: 92,  y: 1,
      xanchor: 'left',   yanchor: 'top',   text: 'Apr',  showarrow: false, font: {size:10}
    },
    { xref: 'x',  yref: 'paper',   x: 122,  y: 1,
      xanchor: 'left',   yanchor: 'top',   text: 'May',  showarrow: false, font: {size:10}
    },
    { xref: 'x',  yref: 'paper',   x: 153,  y: 1,
      xanchor: 'left',   yanchor: 'top',   text: 'Jun',  showarrow: false, font: {size:10}
    },
    { xref: 'x',  yref: 'paper',   x: 183,  y: 1,
      xanchor: 'left',   yanchor: 'top',   text: 'Jul',  showarrow: false, font: {size:10}
    },
    { xref: 'x',  yref: 'paper',   x: 214,  y: 1,
      xanchor: 'left',   yanchor: 'top',   text: 'Aug',  showarrow: false, font: {size:10}
    },
    { xref: 'x',  yref: 'paper',   x: 245,  y: 1,
      xanchor: 'left',   yanchor: 'top',   text: 'Sep',  showarrow: false, font: {size:10}
    },
    { xref: 'x',  yref: 'paper',   x: 275,  y: 1,
      xanchor: 'left',   yanchor: 'top',   text: 'Oct',  showarrow: false, font: {size:10}
    },
    { xref: 'x',  yref: 'paper',   x: 306,  y: 1,
      xanchor: 'left',   yanchor: 'top',   text: 'Nov',  showarrow: false, font: {size:10}
    },
    { xref: 'x',  yref: 'paper',   x: 336,  y: 1,
      xanchor: 'left',   yanchor: 'top',   text: 'Dec',  showarrow: false, font: {size:10}
    }
    ]

  };
  Plotly.newPlot('pcpCumPlot', cumdata, layout);
}); //end d3


} //end plotPrecipitation function


