## Assigns two end nodes to same group. 
##
## Inputs:
## a, b: positions of the two end nodes
## joined.to: for each end node, the end node it was joined to (itself
##   if it has not been joined)
##
## Returns joined.to, updated so a and b have the same node group label.

join_node_labels <- function(a, b, joined.to) {
	node.label.a <- find_node_label(a, joined.to)
	node.label.b <- find_node_label(b, joined.to)
	if (node.label.a != node.label.b) joined.to[node.label.b] <- node.label.a
	joined.to
}
