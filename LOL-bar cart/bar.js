var svg = d3.select("svg"),
    margin = 200,
    width = svg.attr("width") - margin,
    height = svg.attr("height") - margin;
d3.select("#barChart").append("svg")
    .attr("width", width)
    .attr("height", height)
    .attr("id", "primarySVG");