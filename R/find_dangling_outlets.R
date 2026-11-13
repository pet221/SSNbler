## Finds Dangling Nodes at outlets: outlets that probably should connect
## to a nearby line from a different network. Lines in an outlet's own 
## network are ignored. 
##
## Inputs:
## outlets: sf POINT object of the Outlet nodes
## in.edges: sf LINESTRING object of the streams
## topo_tolerance: search distance (map units)
## outlet.network: numeric vector of network numbers for each outlet
## edge.network: numeric vector of network numbers for each line in 
##   in.edges
##
## Returns an integer vector of row numbers corresponding to potential
## dangling outlet nodes

find_dangling_outlets <- function(outlets, 
																	in.edges, 
																	topo_tolerance,
                                  outlet.network, 
																	edge.network) {

	## For each outlet, find lines within topo_tolerance
	buff.outlets <- st_buffer(outlets, topo_tolerance)
	near.edges <- st_intersects(buff.outlets, in.edges)

	## One row per outlet-line pair
	outlet.row <- rep(seq_along(near.edges), lengths(near.edges))
	edge.row <- unlist(near.edges)

	## Pairs where the line belongs to a different network
	other.network.ind <- edge.network[edge.row] != outlet.network[outlet.row]

	## sort and return row numbers of potential outlet errors
	sort(unique(outlet.row[other.network.ind]))
}
