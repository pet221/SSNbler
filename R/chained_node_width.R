## Calculates the width of each chained node: the largest distance
## between end nodes. Used by lines_to_lsn() for the
## node_width column in node_errors.gpkg.
##
## Inputs:
## chained.nodes: integer vector of pointids for Chained Nodes, in 
##   ascending order (from find_chained_nodes)
## node.of.end: integer vector of pointids for the node each line end  
##   node is associated with, in the same order as end.xy
## end.xy: matrix of line end node coordinates in the order from1, to1,
##   from2, to2, ...
##
## Returns a numeric vector of widths (map units), in the same order as
## chained.nodes.

chained_node_width <- function(chained.nodes, node.of.end, end.xy) {

	if (length(chained.nodes) == 0) return(numeric(0))

	## Was end node snapped to chained node?
	chained.end.ind <- node.of.end %in% chained.nodes

	## List w/one element per chained node containing associated row
	## numbers in end.xy (end node coords)
	ends.by.node <- split(which(chained.end.ind),
												factor(node.of.end[chained.end.ind], 
															 levels = chained.nodes))

	## Max distance between end nodes associated w/each chained node
	chained.width <- numeric(length(chained.nodes))
	for (i in seq_along(ends.by.node)) {
		xy <- end.xy[ends.by.node[[i]], , drop = FALSE]

		## Only keep end nodes on the convex hull - protects against
		## chained nodes associated with large number of end nodes.
		if (nrow(xy) > 3) xy <- xy[chull(xy), , drop = FALSE]

		chained.width[i] <- max(sqrt(outer(xy[, 1], xy[, 1], "-")^2 +
																	 outer(xy[, 2], xy[, 2], "-")^2))
	}

	chained.width
}
