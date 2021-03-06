// global namespace for external access
xrefgui = {};

var width = window.innerWidth,
	height = window.innerHeight;

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);

svg.append("svg:marker")
    .attr("id", "arrowhead")
    .attr("viewBox", "0 -5 10 10")
    .attr("refX", 25)
    .attr("refY", 0)
    .attr("markerWidth", 8)
    .attr("markerHeight", 12)
    .attr("orient", "auto")
    .append("svg:path")
    .attr("d", "M0,-5 L10,0 L0,5");
    
var nodeData = [],
	linkData = [];

var forceLayout = d3.layout.force()
    .nodes(nodeData)
    .links(linkData)
    .size([width, height])
    .linkDistance(function(link) {
    	return 60+Math.abs(link.source.depth-link.target.depth)*20;
    })
    .linkStrength(0.8)
    .charge([-300])
    .start();

var linkBindingSelection = svg.selectAll(".link"),
    nodeBindingSelection = svg.selectAll(".node");

function update() {
	linkBindingSelection = linkBindingSelection.data(linkData);
	
	linkBindingSelection.enter()
		.append("line")
			.attr("class", "link")
			.attr("marker-end", "url(#arrowhead)");
	
	nodeBindingSelection = nodeBindingSelection.data(nodeData);
	var newNode = nodeBindingSelection.enter()
		.append("g")
	    	.attr("class", "node")
			.on("click", expandCallees)
			.call(forceLayout.drag);
	
	newNode.append("ellipse")
		.attr("rx", 35)
		.attr("ry", 8)
		.style("fill", "beige");

	var newNodeLabel = newNode.append("text")
		.text(function(node) { return node.name; });
		
	forceLayout.start();
}

forceLayout.on("tick", function(e) {
	// Push sources up and targets down to form a weak tree.
	var k = 6 * e.alpha;
	linkData.forEach(function(d, i) {
	    d.source.y -= k*(Math.abs(d.source.depth-maxDepth/2));
	    d.target.y += k*(Math.abs(d.target.depth-maxDepth/2));
	});
    
	linkBindingSelection
		.attr("x1", function(d) { return d.source.x; })
		.attr("y1", function(d) { return d.source.y; })
		.attr("x2", function(d) { return d.target.x; })
		.attr("y2", function(d) { return d.target.y; });

	nodeBindingSelection.attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });
});

update();

var moduleDeps = [
	{"caller": "some_page",
	 "callee": "some_biz"},
	{"caller": "other_page",
	 "callee": "some_biz"},
	{"caller": "other_page",
	 "callee": "other_biz"},
	{"caller": "some_page",
	 "callee": "some_db"},
	{"caller": "some_biz",
	 "callee": "some_db"},
	{"caller": "other_biz",
	 "callee": "some_db"},
	{"caller": "some_biz",
	 "callee": "other_db"},
	{"caller": "other_db",
	 "callee": "other_page"},
	{"caller": "some_biz",
	 "callee": "other_page"},
];


var nodeMap = {};
var linkMap = {};

moduleDeps.forEach(function (depRel) {
	var sourceName = depRel.caller;
	var targetName = depRel.callee;
	var source, target;
	
	if (nodeMap[sourceName] === undefined)
		source = newNode(sourceName);
	else
		source = nodeMap[sourceName];

	if (nodeMap[targetName] === undefined)
		target = newNode(targetName);
	else
		target = nodeMap[targetName];

	source.callees.push(target);
	target.callers.push(source);

	if (linkMap[linkKey(source, target)] === undefined) {
		newLink(source, target);
	}
});


function newNode(name) {
	var node = {
		"visible" : false,
		"name" : name,
		"callees" : [],
		"callers" : [],
	};
	nodeMap[name] = node;
	return node;
};

function modByName(modName) {
	if (nodeMap[modName] !== undefined) {
		return nodeMap[modName];
	} else {
		return false;
	}
};

function newLink(source, target) {
	var link = {
		"source" : source,
		"target" : target,
		"visible" : false,
	};
	linkMap[linkKey(source, target)] = link;
	return link;
}

function linkKey(source, target) {
	return source.name + "-->" + target.name;
}

function linkByMods(source, target) {
	var key = linkKey(source, target);
	if (linkMap[key] !== undefined)
		return linkMap[key];
	else
		return false;
}

function showMod(mod) {
	if (! mod.visible) {
		nodeData.push(mod);
		mod.visible = true;
		mod.callees.forEach(function(callee) {
			if (callee.visible) {
				showLink(mod, callee);
			}
		});
		mod.callers.forEach(function(caller) {
			if (caller.visible) {
				showLink(caller, mod);
			}
		});
		return true;
	} else {
		return false;
	}
};

function showLink(source, target) {
	var link = linkByMods(source, target);
	if (! link.visible) {
		linkData.push(link);
		link.visible = true;
		return true;
	} else {
		return false;
	}
}

function expandCallees(mod) {
	mod.callees.forEach(function(callee) {
		showMod(callee);
		showLink(mod, callee);
	});
	update();
};

/*************************************************************
 * STRONGLY CONNECTED COMPONENTS
 * We need strongly connected components to be able to find the depth of nodes
 * despite the graph being cyclic.:
 * http://stackoverflow.com/questions/3603274/traversal-of-cyclic-directed-graph
 * We use dijkstra's:
 * http://en.wikipedia.org/wiki/Path-based_strong_component_algorithm
 * 
 * We need the depth to position deep nodes lower than shallow nodes.
 */
var vertices = [];
var C = 0;
var P = [], S = [];
var components = [];

function traverse(v) {
	v["preorder"] = C;
	C++;
	S.push(v);
	P.push(v);
	
	v.callees.forEach(function(w) {
		if (w.preorder === undefined) {
			traverse(w);
		} else {
			if (w.scc === undefined) {
				while (P[P.length-1].preorder > w.preorder) {
					var popped = P.pop();
				}
			}
		}
	});
	if (v === P[P.length-1]) {
		var newComponent = [];
		var popped;
		do {
			popped = S.pop();
			newComponent.push(popped);
			popped["scc"] = newComponent;
		} while (v !== popped);
		components.push(newComponent);
		P.pop();
	}
}

Object.keys(nodeMap).forEach(function(nodeKey) {
	vertices.push(nodeMap[nodeKey]);
});

vertices.forEach(function(vertex) {
	if (vertex.preorder === undefined)
		traverse(vertex);
});
/*
 * END strongly connected components
 ********************************************************/

/**
 * Next: Depth first search over strongly connected components.
 * Each node in an SCC have the same depth.
 * Assign depth to each node.
 * Use relative depth to nudge nodes up/down as in http://jsfiddle.net/jbothma/Gvuz9/23/
 */

//var changed = false;
function deepenNode(vertex, depth) {
	if (vertex.depth === undefined || vertex.depth < depth) {
		vertex.depth = depth;
//		changed = true;
//		console.log("set depth", vertex.name, depth);
	}
	vertex.callees.forEach(function(other) {
		if (other.scc === vertex.scc) {
			// deepen an SCC callee
			var SCCDepth = depth;
//			console.log("initialize SCC depth to", depth);
			other.scc.forEach(function(vInSameSCC) {
				if (other.depth !== undefined && other.depth > SCCDepth) {
					SCCDepth = other.depth;
//					console.log("deepen SCC to ", other.depth);
				}
			});
			other.scc.forEach(function(vInSameSCC) {
				vInSameSCC.depth = SCCDepth;
//				console.log("set scc node depth", vertex.name, SCCDepth);
				// try and recurse out of SCC
				vInSameSCC.callees.forEach(function(SCCCallee) {
					if (SCCCallee.scc !== vInSameSCC.scc) {
//						console.log("exiting SCC", vInSameSCC.name, SCCCallee.name);
						deepenNode(SCCCallee, SCCDepth+1);
					}
				});
			});
		} else {
			// deepen a non-SCC callee
			deepenNode(other, depth+1);
		}
	});
}
//do {
	vertices.forEach(function(vertex) {
		deepenNode(vertex, 0);
	});
//} while (changed);
var maxDepth = 0;
vertices.forEach(function(vertex) {
	console.log(vertex.name, vertex.depth);
	if (vertex.depth > maxDepth)
		maxDepth = vertex.depth;
});
/*
 * END calculating node depth
 ********************************************************************/


var modName = prompt("Starting mod", "some_page");

var mod;
if (mod = modByName(modName)) {
	showMod(mod);
	update();
}
