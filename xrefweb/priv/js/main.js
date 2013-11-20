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
	    	.attr("class", "node");

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

nody = {};
nodo = {};
linky = { "source":nody, "target":nodo};
nodeData.push(nody);
nodeData.push(nodo);
linkData.push(linky);
update();
