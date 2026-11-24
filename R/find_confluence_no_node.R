## Finds Confluence Without Node errors (e.g. T junction)
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

	## Only test shared end node locations once. position.id is the
	## position of each end node; position.end is one end node at each
	## position
	end.xy <- st_coordinates(end.points)
	xy.order <- order(end.xy[, 1], end.xy[, 2])
	new.position.ind <- c(TRUE,
		diff(end.xy[xy.order, 1]) != 0 | diff(end.xy[xy.order, 2]) != 0)
	position.id <- integer(nrow(end.xy))
	position.id[xy.order] <- cumsum(new.position.ind)
	position.end <- xy.order[new.position.ind]

	## Exact predicates miss end nodes due to rounding errors.
	## Make each position a small square using eps, find the lines it
	## touches, then copy the result to every end node at that position
	position.boxes <- st_buffer(end.points[position.end], eps,
		endCapStyle = "SQUARE")
	position.on.line <- st_intersects(position.boxes, in.edges)
	end.on.line <- position.on.line[position.id]

	## One row per end node (k) and line it touches (j). The end node's
	## node is connected to line j if it contains one of j's end nodes
	## (rows 2j - 1 and 2j; always true for the end node's own line)
	k <- rep(seq_along(end.points), lengths(end.on.line))
	j <- unlist(end.on.line)
	k.node <- node.of.end[k]
	not.connected <- k.node != node.of.end[2 * j - 1] &
		k.node != node.of.end[2 * j]

	## Leave out touches within eps of line j's end nodes (same square as
	## position.boxes), so only touches away from line j's end nodes are
	## counted
	near.from <- abs(end.xy[k, 1] - end.xy[2 * j - 1, 1]) <= eps &
		abs(end.xy[k, 2] - end.xy[2 * j - 1, 2]) <= eps
	near.to <- abs(end.xy[k, 1] - end.xy[2 * j, 1]) <= eps &
		abs(end.xy[k, 2] - end.xy[2 * j, 2]) <= eps
	away.from.ends <- near.from == FALSE & near.to == FALSE

	sort(unique(k.node[not.connected & away.from.ends]))
}
