## Finds Chained Nodes: nodes containing line end nodes that are not
## all within snap_tolerance of each other, because snapping is
## transitive (A-B and B-C within snap_tolerance puts A, B and C in one
## node, even if A-C is not). 
##
## Every two end nodes within snap_tolerance are already a pair in
## end.pairs, so a node containing k end nodes is chained when fewer
## than k(k - 1)/2 of its pairs are in end.pairs. Only pairs whose two
## end nodes ended up in the same node are counted (not blocked pairs).
##
## Inputs:
## end.pairs: two-column matrix of end node indices <= snap_tolerance
##   apart (from grid_pairs())
## node.of.end: integer vector of pointids for each line end node, in 
##   the same order as end.xy (in lines_to_lsn)
## n.nodes: number of nodes
##
## Returns an integer vector of the node ids of Chained Nodes.

find_chained_nodes <- function(end.pairs, node.of.end, n.nodes) {

	## Pairs whose two end nodes are associated with the same node
	same.node.ind <- node.of.end[end.pairs[, 1]] == node.of.end[end.pairs[, 2]]

	## Number of end node pairs within snap_tolerance in each node
	n.pairs.in.node <- tabulate(node.of.end[end.pairs[same.node.ind, 1]], n.nodes)

	## Number of end nodes associated with each node
	n.ends.in.node <- as.numeric(tabulate(node.of.end, n.nodes))

	## Chained if fewer pairs than there would be if every pair of end
	## nodes were within snap_tolerance
	which(n.pairs.in.node < n.ends.in.node * (n.ends.in.node - 1) / 2)
}
