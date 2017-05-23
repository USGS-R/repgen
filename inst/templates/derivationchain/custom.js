//This code assumes var derivations has been set prior

var colorMap = {
		"default" : "gray",
		"ProcessorBasic" : 'green',
		"ProcessorDerived" : 'blue',
		"Rating Model" : 'red',
		"Statistic/Computation" : 'orange'
}

var shapeMap = {
		"default" : 'ellipse',
		"ProcessorBasic" : 'triangle',
		"ProcessorDerived" : 'rectangle'
}


var processorMap = {
		"Rating Model" : 'ratingModel',
		"Statistic/Computation" : 'statDerived'
}

var getTimePeriodEdges = function(nodes) {
	var dateMap = {};
	var dateList = [];
	
	for(var i = 0; i < derivations.length; i++) {
		var periodStartTime = derivations[i].periodStartTime;
		var periodEndTime = derivations[i].periodEndTime;
		
		if(!dateMap[periodStartTime]) {
			dateMap[periodStartTime] = periodStartTime;
			dateList.push(periodStartTime);
		}
		
		if(!dateMap[periodEndTime]) {
			dateMap[periodEndTime] = periodEndTime;
			dateList.push(periodEndTime);
		}
	}
	
	dateList.sort();
	
	return dateList;
}

var makeNode = function(nodeData) {
	var col = colorMap[nodeData.type || "default"];
	var shape = shapeMap[nodeData.type || "default"];

	return { 
		data: { 
			id: nodeData.uniqueId, 
			name: nodeData.identifier, 
			parameter: nodeData.parameter,
			sublocation: nodeData.sublocation,
			type: nodeData.type,
			computation: nodeData.computation,
			processor: nodeData.processedByType,
			weight: 50, 
			faveColor: col, 
			faveShape: shape } }
}

var insertEdges = function(edgeList, nodeData, traversedEdgeMap) {
	for(var i = 0; i < nodeData.inputSeriesUid.length; i++) {
		var fromId = nodeData.inputSeriesUid[i];
		var toId = nodeData.uniqueId;
		var edgeKey = fromId + "-" + toId;

		if(!traversedEdgeMap[edgeKey]) {
			var color = colorMap[nodeData.processedByType || "default"];
			var edge = { data: { source: fromId, target: toId, faveColor: color, strength: 20 } }

			var procType = processorMap[nodeData.processedByType]
			if(procType) {
				edge.classes = procType;
			}
			edgeList.push(edge);
			traversedEdgeMap[edgeKey] = true; //mark this node as created so it's not added to the graph again
		}
	}
}

//returns true if the processor date contains the date
var nodeIncludesDate = function(node, dateString) {
	var nodeStartDate = new Date(node.periodStartTime)
	var nodeEndDate = new Date(node.periodEndTime)
	var date = new Date(dateString)
	
	return date <= nodeEndDate && date > nodeStartDate;
}

var makeDerivationCurve = function(forDateString) {
	var nodes = [];
	var edges = [];
	var traversedEdgeMap = {};
	
	for(var i = 0; i < derivations.length; i++) {
		var n = derivations[i]
		if(nodeIncludesDate(n, forDateString)) {
			nodes.push(makeNode(n))
			insertEdges(edges, n, traversedEdgeMap);
		}
	}
	
	return cytoscape({
		container: document.getElementById('cy'),
	
		layout: {
			padding: 10,
			name: 'breadthfirst',
			directed: true,
		},
	
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
						{ display: "Type", value: n.data('type')  },
						{ display: "Computation", value: n.data('computation')  },
						{ display: "Processor", value: n.data('processor')  }
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
	newButton.html("<input type=\"radio\" value=\"" + periodMarkers[i] + "\"> " + new Date(periodMarkers[i-1]) + " thru " + new Date(periodMarkers[i]))
	if(i == periodMarkers.length - 1) {
		newButton.addClass("active");
	}
	var periodSelected = periodMarkers[i];
	attachPeriodChangeHandler(newButton, periodSelected);
	dateSelector.append(newButton);
}