
pdist_node_coords <- function(node_coords, other_xy, 
															snap_tolerance = NULL,
															node_dir) {
  
	dist_matrix <- dist_xy(node_coords, other_xy)
	# colnames(dist_matrix) <- rownames(other_xy)
	# rownames(dist_matrix) <- rownames(node_coords)
	
	n_flow <- apply(dist_matrix, 1, function(x) sum(x <= snap_tolerance))
	
	snap_check <- apply(dist_matrix, 1, function(x) {
		sum(x > 0 & x <= snap_tolerance)
	})
	
	if(node_dir == "to") {
		## (row, column) of every zero distance, rows numbered within this chunk.
		## Duplicate to nodes are found across all chunks by combine_node_chunks()
		zero_pairs <- which(dist_matrix == 0, arr.ind = TRUE)
		return(out.list = list(n_flow = n_flow, snap_check = snap_check,
													 zero_pairs = zero_pairs))
	} else {
	
		return(out.list = list(n_flow = n_flow, snap_check = snap_check))
	}
	
}
