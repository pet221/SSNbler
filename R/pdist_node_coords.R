
pdist_node_coords <- function(node_coords, other_xy, 
															snap_tolerance = NULL,
															node_dir) {
  
	## proxy::dist() computes distances in double precision
	dist_matrix <- unclass(proxy::dist(as.matrix(node_coords), as.matrix(other_xy)))
	# colnames(dist_matrix) <- rownames(other_xy)
	# rownames(dist_matrix) <- rownames(node_coords)
	
	n_flow <- apply(dist_matrix, 1, function(x) sum(x <= snap_tolerance))
	
	snap_check <- apply(dist_matrix, 1, function(x) {
		sum(x > 0 & x <= snap_tolerance)
	})
	
	if(node_dir == "to") {
		dbl_tonodes <- which(colSums(dist_matrix == 0) > 1)
		unsnapped_tonodes <- which(rowSums(dist_matrix[, dbl_tonodes,
																								 drop = FALSE] == 0) > 0)
		return(out.list = list(n_flow = n_flow, snap_check = snap_check,
													 unsnapped_tonodes = unsnapped_tonodes))
	} else {
	
		return(out.list = list(n_flow = n_flow, snap_check = snap_check))
	}
	
}
