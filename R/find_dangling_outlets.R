## Finds Dangling Nodes at outlets: outlets that probably should connect
## to a nearby line from a different network. Lines in an outlet's own 
## network are ignored. A line from another network is also ignored if 
## its closest point to the outlet is its own outlet.
##
## Inputs:
## outlets: sf POINT object of the Outlet nodes
## in.edges: sf LINESTRING object of the streams
## topo_tolerance: search distance (map units)
## outlet.network: numeric vector of network numbers for each outlet
## edge.network: numeric vector of network numbers for each line in 
##   in.edges
## to.outlet.ind: logical vector, TRUE for each line in in.edges whose
##   downstream end node is an outlet
## to.xy: matrix of the downstream end node coordinates of each line in
##   in.edges
## eps: tolerance for rounding error (map units)
##
## Returns an integer vector of row numbers corresponding to potential
## dangling outlet nodes

find_dangling_outlets <- function(outlets, 
																	in.edges, 
																	topo_tolerance,
                                  outlet.network, 
																	edge.network,
																	to.outlet.ind,
																	to.xy,
																	eps) {

	## For each outlet, find lines within topo_tolerance
	buff.outlets <- st_buffer(outlets, topo_tolerance)
	near.edges <- st_intersects(buff.outlets, in.edges)

	## One row per outlet-line pair
	outlet.row <- rep(seq_along(near.edges), lengths(near.edges))
	edge.row <- unlist(near.edges)

	## Keep pairs where the line belongs to a different network
	other.network.ind <- edge.network[edge.row] != outlet.network[outlet.row]
	outlet.row <- outlet.row[other.network.ind]
	edge.row <- edge.row[other.network.ind]

	## Leave out pairs where the line's closest point to the outlet is
	## the line's own outlet (two outlets close together)
	check.pair <- which(to.outlet.ind[edge.row] == TRUE)
	if (length(check.pair) > 0) {
		closest <- st_nearest_points(st_geometry(outlets)[outlet.row[check.pair]],
			st_geometry(in.edges)[edge.row[check.pair]], pairwise = TRUE)

		## Each result is a 2-point line from the outlet to the closest
		## point on the line; keep the closest point (second row)
		closest.xy <- st_coordinates(closest)
		closest.xy <- closest.xy[seq(2, nrow(closest.xy), by = 2), 1:2,
			drop = FALSE]

		line.to.xy <- to.xy[edge.row[check.pair], , drop = FALSE]
		at.own.outlet.ind <- abs(closest.xy[, 1] - line.to.xy[, 1]) <= eps &
			abs(closest.xy[, 2] - line.to.xy[, 2]) <= eps

		keep.ind <- rep(TRUE, length(outlet.row))
		keep.ind[check.pair[at.own.outlet.ind]] <- FALSE
		outlet.row <- outlet.row[keep.ind]
	}

	## sort and return row numbers of potential outlet errors
	sort(unique(outlet.row))
}
