## Finds Network Without Outlet errors, which are a special case 
## where multiple edges form a loop without any other line flowing 
## out of the nodes. This function returns all nodes on a loop without 
## an outflowing edge. A network with only 1 self-loop line is left
## out, because those are already reported as self-loop errors. 
##
## Inputs:
## node.relate: noderelationships table (data.frame with rid, 
##   fromnode, tonode columns)
## n.nodes: number of nodes
## node.network: numeric vector of network numbers for each node 
## outlet.nodes: logical vector, TRUE for Outlet nodes (one value per
##   node)
##
## Returns an integer vector of node numbers (pointids).

find_network_no_outlet <- function(node.relate, 
																	 n.nodes, 
																	 node.network,
                                   outlet.nodes) {

	## Exit early if no networks without outlet are found
	outlet.networks <- unique(node.network[outlet.nodes])
	no.outlet.ind <- !(node.network %in% outlet.networks)
	if (any(no.outlet.ind) == FALSE) return(integer(0))

  ## Exit early if all lines are self loops
	not.self.loop.ind <- node.relate$fromnode != node.relate$tonode
	if (any(not.self.loop.ind) == FALSE) return(integer(0))
	
	## Directed graph of nodes joined by lines, leaving out 
	## self-loop lines
	flow.graph <- igraph::make_graph(
		as.vector(t(node.relate[not.self.loop.ind, c("fromnode", "tonode")])),
		n = n.nodes, directed = TRUE)

	## Find nodes on a loop that can flow back to themselves (in a
	## connected group of more than one node)
	loop.groups <- igraph::components(flow.graph, mode = "strong")
	on.loop.ind <- loop.groups$csize[loop.groups$membership] > 1

	which(no.outlet.ind & on.loop.ind)
}
