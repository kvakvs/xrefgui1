// global namespace for external access
xrefgui = {};

var w = window.innerWidth,
	h = window.innerHeight;

/**
 * d3 uses node fields index, x, y, px, py, fixed, weight.
 *     and link fields source, target
 */
var nodes = [],
	links = [];

var force = d3.layout.force()
    .nodes(nodes)
    .links(links)
    .size([w, h])
    .start();          // start on create

var link = vis.selectAll("line")
    .data(links)
	.enter().append("line");

var node = vis.selectAll("circle")
    .data(nodes)
	.enter().append("circle")
    	.attr("r", 5);

force.on("tick", function() {
  link.attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; });

  node.attr("cx", function(d) { return d.x; })
      .attr("cy", function(d) { return d.y; });
});