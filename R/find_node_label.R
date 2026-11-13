## Finds the snapped node group id an end node currently belongs to. 
## Called from split_self_loops
##
## Inputs:
## a: position of the end node in the merged parent node 
## parent: for each end node, the end node it was merged into (itself
##   if it has not been merged)
##
## Returns the node group label (a position in parent).

find_node_label <- function(a, parent) {
	while (parent[a] != a) a <- parent[a]
	a
}
