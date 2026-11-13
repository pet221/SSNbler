## Finds Intersection Without Node errors: points where two lines cross
## away from any line end node. 
##
## Two kinds of crossing are not errors, and are left out:
## - Crossings within eps of a line end node are considered rounding
##   errors at a node. The end node is reported by its own check (e.g. 
##   Confluence Without Node, Complex Confluence).
## - Crossings between two lines that are connected at a node, within
##   max(eps, snap_tolerance) of that node. 
## A crossing near the end node of a line that is not connected to the
## other line (e.g. an overshoot) is still reported.
##
## Inputs:
## in.edges: sf LINESTRING object of the streams, with rid = row number
## end.points: sfc POINT geometry for every line end node, in the order
##   from1, to1, from2, to2, ...
## end.xy: matrix of line end node coordinates, in the same order as
##   end.points
## node.of.end: integer vector of pointids for the node each line end  
##   node is associated with, in the same order as end.xy and end.points
## snap_tolerance: snapping distance used to build the nodes (map units)
## eps: tolerance for rounding error (map units)
##
## Returns an sf POINT object (one row per location, with the rids of
## the two crossing lines as its first two columns).

find_intersection_no_node <- function(in.edges, 
																			end.points, 
																			end.xy,
																			node.of.end, 
																			snap_tolerance, 
																			eps) {

	## Keep only rid, so the intersections have one rid column per line
	in.edges <- in.edges[, "rid"]

	## Prevents st_intersection from printing unnecessary warning
	st_agr(in.edges) <- "constant"

	## Find candidate lines whose interiors cross
	cross.edges <- which(lengths(st_crosses(in.edges, in.edges)) > 0)
	cand <- in.edges[cross.edges, ]
	intersections <- st_intersection(cand, cand)
	st_agr(intersections) <- "constant"

	## Keep the crossing points. Two lines that cross more than once give
	## a MULTIPOINT. A crossing plus an overlap gives a
	## GEOMETRYCOLLECTION. Split these into single points
	geom.type <- st_geometry_type(intersections)
	points.only <- intersections[geom.type == "POINT", ]
	multi.points <- intersections[geom.type == "MULTIPOINT", ]
	collections <- intersections[geom.type == "GEOMETRYCOLLECTION", ]
	
	if (nrow(collections) > 0) {
		collections <- st_collection_extract(collections, "POINT")
		collection.type <- st_geometry_type(collections)
		points.only <- rbind(points.only,
												 collections[collection.type == "POINT", ])
		multi.points <- rbind(multi.points,
													collections[collection.type == "MULTIPOINT", ])
	}
	if (nrow(multi.points) > 0) {
		points.only <- rbind(points.only, st_cast(multi.points, "POINT"))
	}

	## Leave out crossings within eps of a line end node
	near.end <- st_intersects(st_buffer(points.only, eps), end.points)
	ill.int <- points.only[lengths(near.end) == 0, ]

	## Leave out crossings between lines connected at a node, if within
	## max(eps, snap_tolerance) of that node. Line r's
	## end nodes are rows 2r - 1 (from) and 2r (to) of end.xy
	if (nrow(ill.int) > 0) {
		cross.xy <- st_coordinates(ill.int)
		## rids of the two crossing lines (first two attribute columns)
		cross.rids <- st_drop_geometry(ill.int)
		rid.a <- cross.rids[[1]]
		rid.b <- cross.rids[[2]]
		connect.tol <- max(eps, snap_tolerance)
		connected.ind <- logical(nrow(ill.int))

		## Check each combo of end nodes from line a and line b
		for (i in 1:2) {
			for (j in 1:2) {
				end.a <- 2 * rid.a - 2 + i
				end.b <- 2 * rid.b - 2 + j
				same.node.ind <- node.of.end[end.a] == node.of.end[end.b]
				dist.a <- sqrt((cross.xy[, 1] - end.xy[end.a, 1])^2 +
											 (cross.xy[, 2] - end.xy[end.a, 2])^2)
				dist.b <- sqrt((cross.xy[, 1] - end.xy[end.b, 1])^2 +
											 (cross.xy[, 2] - end.xy[end.b, 2])^2)
				connected.ind <- connected.ind | (same.node.ind &
					(dist.a <= connect.tol | dist.b <= connect.tol))
			}
		}
		ill.int <- ill.int[!connected.ind, ]
	}

	## One row per location
	if (nrow(ill.int) > 0) {
		g.col <- attributes(ill.int)$sf_column
		ill.int <- ill.int[!duplicated(ill.int[, g.col]), ]
	}

	ill.int
}
