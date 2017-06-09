//This code assumes var derivations has been set prior

var colorMap = {
		"default" : "#A9A9A9",
		"ProcessorBasic" : '#0000FF',
		"ProcessorDerived" : '#008000',
		"ratingmodel" : '	#FF0000',
		"statistics" : '#FFA500',
		"calculation" : '#000080',
		"correctedpassthrough" : "#3CB371"
}

var shapeMap = {
		"default" : 'ellipse',
		"ProcessorBasic" : 'triangle',
		"ProcessorDerived" : 'rectangle'
}


var processorMap = {
		"ratingmodel" : 'ratingModel',
		"statistics" : 'statDerived',
		"calculation" : 'calculation',
		"correctedpassthrough" : "correctedpassthrough",
		"fillMissingData": "fillmissingdata",
    "conditionalFill": "conditionaldata"
}

var getTimePeriodEdges = function(nodes) {
	var dateMap = {};
	var dateList = [];
	
	for(var i = 0; i < derivations.length; i++) {
		var periodStartTime = derivations[i].periodStartTime;
		var periodEndTime = derivations[i].periodEndTime;

		
		if(periodStartTime && !dateMap[periodStartTime]) {
			dateMap[periodStartTime] = periodStartTime;
			dateList.push(periodStartTime);
		}
		
		if(periodEndTime && !dateMap[periodEndTime]) {
			dateMap[periodEndTime] = periodEndTime;
			dateList.push(periodEndTime);
		}
	}
	
	dateList.sort();
	
	return dateList;
};

var makeNode = function(nodeList, nodeData, insertedNodes) {
  
  var label = nodeData.identifier;
	if(label) {
		label = label.split("@")[0];
	}
	
	for(var i = 0; i < nodeData.inputTimeSeriesUniqueIds.length; i++) {

	  var col = colorMap[nodeData.timeSeriesType || "default"];
	  var shape = shapeMap[nodeData.timeSeriesType || "default"];
	  var node = { 
	    data: { 
  	    id: nodeData.uniqueId, 
  			name: label, 
  			parameter: nodeData.parameter,
  			sublocation: nodeData.sublocation,
  			timeSeriesType: nodeData.timeSeriesType,
  			computation: nodeData.computation,
  			processorType: nodeData.processorType,
  			publish: nodeData.publish,
  			primary: nodeData.primary,
  			weight:50,
  			faveColor: col, 
  			faveShape: shape 
	    } 
	  };
	  
	  var nodeProcessorType = processorMap[nodeData.processorType];
	  if(nodeProcessorType){
	    node.classes = nodeProcessorType;
	  }
	  nodeList.push(node);
	  
	}

	insertedNodes[nodeData.uniqueId] = true;
	
	return { 
		data: { 
			id: nodeData.uniqueId, 
			name: label, 
			parameter: nodeData.parameter,
			sublocation: nodeData.sublocation,
			timeSeriesType: nodeData.timeSeriesType,
			computation: nodeData.computation,
			processorType: nodeData.processorType,
			publish: nodeData.publish,
			primary: nodeData.primary,
			weight: 50, 
			faveColor: col, 
			faveShape: shape } };
};

var insertEdges = function(edgeList, nodeData, traversedEdgeMap, insertedNodes) {
	
	if(!nodeData.inputTimeSeriesUniqueIds) {
		return; //if no inputs, no edges to draw
	}
	for(var i = 0; i < nodeData.inputTimeSeriesUniqueIds.length; i++) {
		var fromId = nodeData.inputTimeSeriesUniqueIds[i];
		var toId = nodeData.uniqueId;
		var edgeKey = fromId + "-" + toId;

		if(!traversedEdgeMap[edgeKey] && insertedNodes[fromId] && insertedNodes[toId]) {
			var color = colorMap[nodeData.processorType || "default"];
			var edge = { data: { source: fromId, target: toId, faveColor: color, strength: 20 } }

			var procType = processorMap[nodeData.processorType]
			if(procType) {
				edge.classes = procType;
			}
			edgeList.push(edge);
			traversedEdgeMap[edgeKey] = true; //mark this node as created so it's not added to the graph again
		}
	}
}

//returns true if the processor range contains the date
var nodeIncludesDate = function(node, dateString) {
	if(!node.periodStartTime || !node.periodEndTime) {
		return false;
	}
	
	var nodeStartDate = new Date(node.periodStartTime)
	var nodeEndDate = new Date(node.periodEndTime)
	var date = new Date(dateString)
	
	return date <= nodeEndDate && date > nodeStartDate;
}

var makeDerivationCurve = function(forDateString) {
	var nodes = [];
	var edges = [];
	var traversedEdgeMap = {};
	var insertedNodes = {};
	
	for(var i = 0; i < derivations.length; i++) {
		var n = derivations[i];
		if(nodeIncludesDate(n, forDateString)) {
			makeNode(nodes, n, insertedNodes);
		}
	}
	
	for(var i = 0; i < derivations.length; i++) {
		var n = derivations[i]
		if(nodeIncludesDate(n, forDateString)) {
			insertEdges(edges, n, traversedEdgeMap, insertedNodes);
		}
	}
	
	var graph = cytoscape({
		container: document.getElementById('cy'),
	
		layout: {
			padding: 10,
			name: 'cose-bilkent',
			directed: true,
			spacingFactor: 1.5,
			nodeDimensionsIncludeLabels: true,
			tile: false
		},
		
		wheelSensitivity: .25,
	  minZoom: .25,
	  maxZoom: 2,
	
		style: cytoscape.stylesheet()
		.selector('node')
		.css({
			'shape': 'data(faveShape)',
			'width': 'mapData(weight, 40, 80, 20, 60)',
			'content': 'data(name)',
			'text-valign': 'center',
			'text-outline-width': 2,
			'text-outline-color': 'data(faveColor)',
			'text-wrap': 'wrap',
			'text-max-width': '200px',
			'background-color': '#fff',
			'background-fit': 'cover',
			'color': '#fff',
			'border-width': 2,
			'border-color': '#333',
			'font-size': 10
		})
		.selector('node.ratingModel')
		.css({
		  'background-image': 'url("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAQVJREFUeNq0kt0NgjAUhYvps2EE2IARcAM3EF+Mb8gE6gToG/FFNgAngBHqBoxAXABP4dZUpERivMnJSX++tvfeMvZjWFM2zy/MgSlVjw0r+ReQBwshn0AVpRQfASWwJ9AY3ADHsN3AUgUJ6E7+XgOANqyAPG26hnLojJyFsYgGWIIRwKodJc2S1gXbWjnGgX5A0ct3DTAlUEKZqr7uM4IPRriLjFJxcbMLP6qOcOptqG2OejAjoARc9eaYhQOu8ED1FvBiyufiGtze/rGjyz+mkSxepBV0xelHyfzzoTYhbK0+N4A+dUJ+spTLJ1Md6pGXCroo1uZOr9eMRtI4kM3+FU8BBgBg1FIcIRmAgwAAAABJRU5ErkJggg==")'
		})
		.selector('node.calculation')
		.css({
		  'background-image': 'url("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAT1JREFUeNpiZACC/zONDYDUeiBWYEAFB6C0A5r4AyAOZEw/e4ERqnk/EAswYAJcBoDAByB2ZAIS9XDNlsUMDGq+DEQCkJ56FiBhAOYapwE9ALRIRA0ifWszqnKQwbySEPbn5zB5Axa4v6WMGRjYeUAMBgZhNUz7QGIww9/cgokqgMLgP1yR70wGhmdnGRjOziImDMCABYW3OZ2BVMCCZAs2cIEYA/CCSTwtBA1wIMKlRIQBLP5BoQ2Kore3UBRqKkkz8HFzgtmfvn5nuH7vKZjNhGoPME2p+5AUiEzQdA2x+ddnBgY2XmCCkcJQKCrIxyAjLgTGIDYsT7BAQ1oB7GwQBmkGpQU0AHLyPTaIj3/++gOPJZBIIziQ3t6C5Ac0v8PA6/efsGWmRiZQlgTlKrhXiAMgtY750Z4XAAIMAC2LUPlNin0qAAAAAElFTkSuQmCC")'
		})
		.selector('node.correctedpassthrough')
		.css({
		  'background-image': 'url("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAP1JREFUeNpidHFx+c9AAtizZw8jMp8FShcC8QUCeg2AuB9dEGbABaDJB/DpBroUqzgTModvFoMDEAuQ4iUWLGL9QNtAXqoH4gIsLvmPz4APQJxwKmwPw6c0hkRo2MBcJwAU+4BuGIoXgApAAfkAZAhQw3qYd0Bew+YajDCAgkYoHQDE94GaG4B0PBDnYwsfDAPMVoFDewGUKwANiwQkNkEXzIf6fwIWuQKgKxSwBaIBejwDDSkEKt4IMhCIFZAC2QAaTiguAKWw/VCMbAhy4gJ5SxEotgHFBehpGzmeoc4FGdII1PiA2ISE7AKQpkR8apgYKAQsODIO0VkcIMAA+QNIBuP+5PMAAAAASUVORK5CYII=")'
		})
		.selector('node.statDerived')
		.css({
		  'background-image': 'url("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAANtJREFUeNpiYBhowEi0yhn/BYBkPRAXIAsz4dGQAMT7gdgAzM9g/AAkDwLxB8IuAGlmYJgP5T0AYkMgDkASg4krMBHQDAIKQHweWUyK92wj1NALTAQ0IxsCBkqCeyb4qKfnQ73kyIRH8wWg4kQ0Pye6KFeA1AnAwoUJl+ZYffcHQMX9IFtAhnCyvt0A1LAArhkeC1g0gzQBNQSAFWcwXgAaVAg0UAFbeDNhsxnqPwYkf4PUGOAy4AKyZqjNRAMWqB8NgLYe4Dxj/J/UpMwCde4BcvMC04DnRoAAAwD3tktTisqW9AAAAABJRU5ErkJggg==")'
		})
		.selector('node.basic')
		.css({
		  'background-image': 'url("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAJtJREFUeNpi/P//PwMlgJFSA5jS09MpMoGJZCfPZEgA4v9AvIBkA0CagdR8KDeeJAPQNINAIohgwaMYhC/8T2cowKYZKL4AqwFoiu2BfAMQjU0zhhew2MSATzOKAVg0NwLxR3ya0b2QgK4YaOgGIDsAGhYbsIUXsgEw0xfAbALSF0Ca8cZOWloaKCU6kpsSYS7YP2CZiWIDAAIMACrvPcolLgvaAAAAAElFTkSuQmCC")'
		})
		.selector('node.fillmissingdata')
		.css({
		  'background-image': 'url("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAPpJREFUeNpiZGi+7sDAwFAPxA8YajUTGUgETEDsAMUJQMMUyDFgAhB/gPLrSTegVhOkeSKUT7IrmKA0QVeknTEOwG0Aca5YDzRkPxAbYHMBA5FhAQrs80BD5gOxAEiAEUW6+XoDSDMv5xsGYb6HDJaaSxh4OV/j8j7IskIWZJH/nppw9i0GXoYFDAoMbxnYcBlwAYgPwF3w/wwDyEn3gVjgCQPnhmYGrYloGvZD6QdAnDjL5OwBFFmgAQ1A/B+KFbDEwnsgLsDqFpDtQPweqnk+jmgUwJkYCNmOFxBjO6GUCPIXzHmNpBoAikZQaNqDQpfRBBzCJAGAAAMAN2JnMVIyij8AAAAASUVORK5CYII=")'
		})
		.selector('node.conditionaldata')
		.css({
		  'background-image': 'url("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAPpJREFUeNpiZGi+7sDAwFAPxA8YajUTGUgETEDsAMUJQMMUyDFgAhB/gPLrSTegVhOkeSKUT7IrmKA0QVeknTEOwG0Aca5YDzRkPxAbYHMBA5FhAQrs80BD5gOxAEiAEUW6+XoDSDMv5xsGYb6HDJaaSxh4OV/j8j7IskIWZJH/nppw9i0GXoYFDAoMbxnYcBlwAYgPwF3w/wwDyEn3gVjgCQPnhmYGrYloGvZD6QdAnDjL5OwBFFmgAQ1A/B+KFbDEwnsgLsDqFpDtQPweqnk+jmgUwJkYCNmOFxBjO6GUCPIXzHmNpBoAikZQaNqDQpfRBBzCJAGAAAMAN2JnMVIyij8AAAAASUVORK5CYII=")'
		})
		.selector(':selected')
		.css({
			'border-width': 2,
			'border-color': '#dd0000'
		})
		.selector('edge')
		.css({
			'curve-style': 'bezier',
			'opacity': 0.666,
			'width': 'mapData(strength, 70, 100, 2, 6)',
			'target-arrow-shape': 'triangle',
			'source-arrow-shape': 'circle',
			'line-color': 'data(faveColor)',
			'source-arrow-color': 'data(faveColor)',
			'target-arrow-color': 'data(faveColor)'
		})
		.selector('edge.ratingModel')
		.css({
			'line-style': 'solid',
		})
		.selector('edge.calculation')
		.css({
			'line-style': 'solid',
		})
		.selector('edge.correctedpassthrough')
		.css({
			'line-style': 'solid',
		})
		.selector('edge.statDerived')
		.css({
			'line-style': 'dotted',
		})
		.selector('.faded')
		.css({
			'opacity': 0.25,
			'text-opacity': 0
		}),
		
		elements: {
			nodes: nodes,
			edges: edges
		},
	
		ready: function(){
			var cy = window.cy = this;
	
			cy.nodes().forEach(function(n){
				n.qtip({
					content: [
						{ display: "Parameter", value: n.data('parameter')  },
						{ display: "Sublocation", value: n.data('sublocation')  },
						{ display: "Type", value: n.data('timeSeriesType')  },
						{ display: "Computation", value: n.data('computation')  },
						{ display: "Processor", value: n.data('processorType') },
						{ display: "Publish", value: n.data('publish')  },
						{ display: "Primary", value: n.data('primary')  }
						].map(function( link ){
							return '<span>' + link.display + ' : ' + link.value + '</span>';
						}).join('<br />\n'),
						position: {
							my: 'top center',
							at: 'bottom center'
						},
						style: {
							classes: 'qtip-bootstrap',
							tip: {
								width: 16,
								height: 8,
							}
						}
				});
			});
		}
	});
	
	var _graph = graph;
	graph.on("zoom", function() {
		$("#zoomLevel").html(Math.floor(_graph.zoom()*100));
	});
	return graph;
}

var periodMarkers = getTimePeriodEdges(derivations);


var cy = makeDerivationCurve(periodMarkers[periodMarkers.length-1]);


function attachPeriodChangeHandler(newButton, inDateSelected) {
	var dateSelected = inDateSelected;
	newButton.click(function() {
		if(cy) {
			cy.destroy();
			$("#cy").html();
		}
		cy = makeDerivationCurve(dateSelected);
	})
}

var dateSelector = $("#dateSelector");
for(var i = periodMarkers.length - 1; i > 0 ; i--) {
	var newButton = $("<label>");
	newButton.addClass("btn");
	newButton.addClass("btn-default");
	newButton.html("<input type=\"radio\" value=\"" + periodMarkers[i-1] + "\"> " + new Date(periodMarkers[i-1]).toLocaleString() + " thru " + new Date(periodMarkers[i]).toLocaleString())
	if(i == periodMarkers.length - 1) {
		newButton.addClass("active");
	}
	var periodSelected = periodMarkers[i];
	attachPeriodChangeHandler(newButton, periodSelected);
	dateSelector.append(newButton);
}