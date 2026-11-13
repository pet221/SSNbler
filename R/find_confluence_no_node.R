## Finds Confluence Without Node errors (e.g. T junction): nodes with
## a line end node on, or within eps of, a line that the node is not
## connected to. Used by lines_to_lsn().
##
## Inputs:
## end.points: sfc POINT geometry of every line end node, in the order
##   from1, to1, from2, to2, ...
## in.edges: sf LINESTRING object of streams
## node.of.end: integer vector of pointids for the node each line end  
##   node is associated with, in the same order as end.xy
## eps: tolerance for rounding error (map units)
##
## Returns an integer vector of pointids, de-duplicated and sorted.

find_confluence_no_node <- function(end.points, in.edges, node.of.end, eps) {

	## Exact predicates miss end nodes due to rounding errors.
	## Make end node a small square using eps
	end.boxes <- st_buffer(end.points, eps, endCapStyle = "SQUARE")
	end.on.line <- st_intersects(end.boxes, in.edges)

	## One row per end node (k) and line it touches (j). The end node's
	## node is connected to line j if it contains one of j's end nodes
	## (rows 2j - 1 and 2j; always true for the end node's own line)
	k <- rep(seq_along(end.points), lengths(end.on.line))
	j <- unlist(end.on.line)
	k.node <- node.of.end[k]
	not.connected <- k.node != node.of.end[2 * j - 1] &
		k.node != node.of.end[2 * j]

	sort(unique(k.node[not.connected]))
}
