var w = window.innerWidth,
    h = window.innerHeight-40,
    r = 6,
    fill = d3.scale.category20();

var force = d3.layout.force()
    .charge(-150)
    .linkDistance(50)
    .size([w, h]);

var svg = d3.select("body").append("svg:svg")
    .attr("width", w)
    .attr("height", h)
    .attr("style", "border: 1px solid black");

nodes = [
    {"name": "d3"},
    {"name": "d3.svg"},
    {"name": "d3.svg.area"},
    {"name": "d3.svg.line"},
    {"name": "d3.scale"},
    {"name": "d3.scale.linear"},
    {"name": "d3.scale.ordinal"}
];

links = [
    {"source": 0, "target": 1},
    {"source": 1, "target": 2},
    {"source": 1, "target": 3},
    {"source": 0, "target": 4},
    {"source": 4, "target": 5},
    {"source": 4, "target": 6}
];

svg.append("svg:marker")    
    .attr("id", "arrowhead")
    .attr("viewBox", "0 -5 10 10")
    .attr("refX", 15)
    .attr("refY", -1.5)
    .attr("markerWidth", 6)
    .attr("markerHeight", 6)
    .attr("orient", "auto")
    .append("svg:path")
    .attr("d", "M0,-5L10,0L0,5");

// Add link lines with arrowhead end-markers
var link = svg.selectAll("line")
    .data(links)
    .enter().append("svg:line")
    .attr("marker-end", function(d) {
        return "url(#arrowhead)";
    });

// Add nodes
var node = svg.selectAll("circle")
    .data(nodes)
    .enter().append("svg:circle")
    .attr("r", r - .75)
    .style("fill", function(d) { return fill(d.group); })
    .style("stroke", function(d) {
        return d3.rgb(fill(d.group)).darker();
    })
    .call(force.drag);
    
svg.selectAll("circle")
    .data(nodes)
    .enter()
    .append("svg:text").text(function(d, i) {
		return d.name;
	});

//Add the SVG Text Element to the svgContainer
var text = svg.selectAll("text")
              .data(nodes)
              .enter()
              .append("text");

//Add SVG Text Element Attributes
var textLabels = text
                 .attr("x", function(d) { return d.cx; })
                 .attr("y", function(d) { return d.cy; })
                 .text( function (d) { return d.name; })
                 .attr("font-family", "sans-serif")
                 .attr("font-size", "15px");

force.nodes(nodes)
    .links(links)
    .on("tick", tick)
    .start();

var pi = 3.142;

function tick(e) {
    // Push sources up and targets down to form a weak tree.
    var k = 6 * e.alpha;
    links.forEach(function(d, i) {
        d.source.y -= k;
        d.target.y += k;
    });

    node.attr("cx", function(d) { return d.x; })
        .attr("cy", function(d) { return d.y; });

    link.attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; });
        
    text.attr("x", function(d) { return d.x; })
        .attr("y", function(d) { return d.y; });
};
