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
    .linkDistance(80)
    .charge([-100])
    .start();          // start on create

var linkBindingSelection = svg.selectAll(".link"),
    nodeBindingSelection = svg.selectAll(".node");

function update() {
	linkBindingSelection = linkBindingSelection.data(linkData);
	
	linkBindingSelection.enter()
		.append("line")
			.attr("class", "link");
	
	nodeBindingSelection = nodeBindingSelection.data(nodeData);
	var newNode = nodeBindingSelection.enter()
		.append("g")
	    	.attr("class", "node")
			.on("click", expandCallees)
			.call(forceLayout.drag);
	
	newNode.append("ellipse")
		.attr("rx", 30)
		.attr("ry", 10)
		.style("fill", "beige");

	var newNodeLabel = newNode.append("text")
		.text(function(node) { return node.name; });
		
	forceLayout.start();
}

forceLayout.on("tick", function() {
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
	return source.name + "_" + target.name;
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

var modName = prompt("Starting mod", "some_page");

var mod;
if (mod = modByName(modName)) {
	showMod(mod);
	update();
}