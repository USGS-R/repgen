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
		"correctedpassthrough" : "correctedpassthrough"
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
}

var makeNode = function(nodeData, insertedNodes) {
	
	var col = colorMap[nodeData.timeSeriesType || "default"];
	var shape = shapeMap[nodeData.timeSeriesType || "default"];

	var label = nodeData.identifier
	if(label) {
		label = label.split("@")[0]
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
			faveShape: shape } }
}

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
		var n = derivations[i]
		if(nodeIncludesDate(n, forDateString)) {
			nodes.push(makeNode(n, insertedNodes))
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
			name: 'cose',
			directed: true,
			spacingFactor: 1,
			nodeDimensionsIncludeLabels: true
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
			'background-color': 'data(faveColor)',
			'color': '#fff'
		})
		.selector(':selected')
		.css({
			'border-width': 3,
			'border-color': '#333'
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
			'target-arrow-shape': 'triangle'
		})
		.selector('edge.calculation')
		.css({
			'line-style': 'solid',
			'target-arrow-shape': 'tee'
		})
		.selector('edge.correctedpassthrough')
		.css({
			'line-style': 'solid',
			'target-arrow-shape': 'triangle-cross'
		})
		.selector('edge.statDerived')
		.css({
			'line-style': 'dotted',
			'target-arrow-shape': 'diamond'
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
						{ display: "Processor", value: n.data('processorType')  },
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
								height: 8
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