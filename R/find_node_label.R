## Finds the snapped node group id an end node currently belongs to. 
## Called from prevent_snapped_loops
##
## Inputs:
## a: position of the end node
## joined.to: for each end node, the end node it was joined to (itself
##   if it has not been joined)
##
## Returns the node group label (a position in joined.to).

find_node_label <- function(a, joined.to) {
	while (joined.to[a] != a) a <- joined.to[a]
	a
}
