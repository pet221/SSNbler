
pdist_rid_confl <- function(to_xy, from_xy, snap_tolerance = NULL) {
	
	dist_matrix <- dist_xy(from_xy, to_xy)
	colnames(dist_matrix) <- rownames(to_xy)
	rownames(dist_matrix) <- rownames(from_xy)
	
	## Returns a list == length(to_xy) with dist_matrix *names* containing
	## the rid value and the values representing the rid for other flow-connected edges connecting to the
	## same node. Does not capture flow-unconnected
	#1= rows, 2=columns
	confl.list <- apply(dist_matrix, 2, function(x) which(x <= snap_tolerance),
											simplify = FALSE)
	
	return(confl.list)
	
}