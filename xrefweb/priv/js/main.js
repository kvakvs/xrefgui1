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
			.attr("id", function(node) { return "mod_svg_" + node.name; })
			.on("click", function(node) { expand(node); });

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

newNode = function(name) {
	return {
		"visible" : false,
		"name" : name,
		"callees" : [],
		"callers" : [],
	};
};

for (var ii = 0; ii < moduleDeps.length; ii++) {
	var sourceName = moduleDeps[ii].source;
	var targetName = moduleDeps[ii].target;
	var source, target;
	
	if (nodeMap[sourceName] === undefined)
		source = nodeMap[sourceName] = newNode(sourceName);
	else
		source = nodeMap[sourceName];
	if (nodeMap[targetName] === undefined)
		target = nodeMap[targetName] = newNode(targetName);
	else
		target = nodeMap[sourceName];
	
	source.callees.push(target);
	target.callers.push(source);
}

function modByName(modName) {
	if (nodeMap[modName] !== undefined) {
		return nodeMap[modName];
	} else {
		return false;
	}
};

function show(mod) {
	if (! mod.visible) {
		nodeData.push(mod);
		mod.visible = true;
		//update();
		return true;
	} else {
		return false;
	}
};

function expand(mod) {
	mod.callees.forEach(function(callee) {
		show(callee);
	});
	update();
};

var modName = prompt("Starting mod", "some_page");

var mod;
if (mod = modByName(modName)) {
	show(mod);
	update();
}



//nody = {};
//nodo = {};
//linky = { "source":nody, "target":nodo};
//nodeData.push(nody);
//nodeData.push(nodo);
//linkData.push(linky);
//update();
