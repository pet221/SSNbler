get_pdist_nodes<- function(other_xy,
													 node_xy,
													 ncores,
													 snap_tolerance,
													 node_dir) {

  node.index<-NULL

	# 2.1 GB = 2.1 x 1024^3 = 2256452608
	# 8 bites per element
	# 2256452608/8 = 282031744 max elements
	## Check number of elements 282000000
	## 282,031,744 elements max for 2.1Gb
	max.rows <- floor(280000000/nrow(other_xy))/2
	if(max.rows > 500) max.rows <- 500

	chunk.no <- ceiling(nrow(node_xy)/max.rows)
	#if(chunk.no < 1) chunk.no <- 1

	cl <- makeCluster(ncores)
	registerDoParallel(cl)

	## close cluster upon function exit
	on.exit({
		stopCluster(cl)
		suppressWarnings(closeAllConnections())
	})

	# Split node_xy into a list of matrices
	node_xy_list <- split_matrix(node_xy, chunk.no)

	## Calculate distances, identify FC edges that share end node,
	## return as list of rids or counts
	out.list<- foreach(node.index = 1:length(node_xy_list),
													.combine = 'c',
													.inorder = TRUE,
													.packages = c("SSNbler"),
													.export = c("pdist_node_coords"),
													.errorhandling = "stop") %dopar% {

														pdist_node_coords(node_coords = node_xy_list[[node.index]],
																							other_xy = other_xy,
																							snap_tolerance = snap_tolerance,
																							node_dir = node_dir)
													}

	n_flow <- unlist(lapply(seq_along(out.list), function(i) {
		if (names(out.list)[i] == "n_flow") out.list[[i]]
	}))

	snap_check <- unlist(lapply(seq_along(out.list), function(i) {
		if (names(out.list)[i] == "snap_check") out.list[[i]]
	}))
	
	unsnapped_tonodes <- unlist(lapply(seq_along(out.list), function(i) {
		if (names(out.list)[i] == "unsnapped_tonodes") out.list[[i]]
	}))

	return(list(n_flow = n_flow, snap_check = snap_check, 
							unsnapped_tonodes = unsnapped_tonodes))
}
