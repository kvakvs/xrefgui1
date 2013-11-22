// global namespace for external access
xrefgui = {};

var width = window.innerWidth,
	height = window.innerHeight;

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);
    
/**
 * d3 uses node fields index, x, y, px, py, fixed, weight.
 *     and link fields source, target
 */
var nodeData = [],
	linkData = [];

var forceLayout = d3.layout.force()
    .nodes(nodeData)
    .links(linkData)
    .size([width, height])
    .linkDistance(30)
    .start();          // start on create

var linkBindingSelection,
    nodeBindingSelection;

function update() {
	linkBindingSelection = svg.selectAll(".link")
	    .data(linkData)
		.enter().append("line")
			.attr("class", "link");
	
	nodeBindingSelection = svg.selectAll(".node")
	    .data(nodeData)
		.enter().append("circle")
	    	.attr("r", 5)
	    	.attr("class", "node")
			.attr("id", function(node) { return "mod_svg_" + node.name; });

	forceLayout.start();
}

forceLayout.on("tick", function() {
  linkBindingSelection
      .attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; });

  nodeBindingSelection
      .attr("cx", function(d) { return d.x; })
      .attr("cy", function(d) { return d.y; });
});

update();

show = function(modName) {
	if (nodeMap[modName] !== undefined && !(nodeMap[modName].visible)) {
		nodeData.push(nodeMap[modName]);
		update();
		return true;
	} else {
		return false;
	}
};

newNode = function(name) {
	return {
		"visible" : false,
		"name" : name
	};
};

var moduleDeps = [
	{"source": "some_page",
	 "target": "some_biz"},
	{"source": "other_page",
	 "target": "some_biz"},
	{"source": "other_page",
	 "target": "other_biz"},
	{"source": "some_page",
	 "target": "some_db"},
	{"source": "some_biz",
	 "target": "some_db"},
	{"source": "other_biz",
	 "target": "some_db"}
];

var nodeMap = {};

for (var ii = 0; ii < moduleDeps.length; ii++) {
	var sourceName = moduleDeps[ii].source;
	var targetName = moduleDeps[ii].target;
	
	if (nodeMap[sourceName] === undefined)
		nodeMap[sourceName] = newNode(sourceName);
	if (nodeMap[targetName] === undefined)
		nodeMap[targetName] = newNode(targetName);
	
	
}


var startingMod = prompt("Starting mod", "some_page");
show(startingMod);



//nody = {};
//nodo = {};
//linky = { "source":nody, "target":nodo};
//nodeData.push(nody);
//nodeData.push(nodo);
//linkData.push(linky);
//update();
