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
	## return a list with one element per chunk
	out.list<- foreach(node.index = 1:length(node_xy_list),
													.inorder = TRUE,
													.packages = c("SSNbler"),
													.export = c("pdist_node_coords"),
													.errorhandling = "stop") %dopar% {

														pdist_node_coords(node_coords = node_xy_list[[node.index]],
																							other_xy = other_xy,
																							snap_tolerance = snap_tolerance,
																							node_dir = node_dir)
													}

	return(combine_node_chunks(out.list))
}

## Combine the per-chunk results of pdist_node_coords() into the vectors the serial
## path computes from the whole node matrix. Rows in each chunk's zero_pairs are
## offset by the rows of the chunks before it, and duplicate to nodes are counted
## over all chunks, so a pair split between two chunks is found.
combine_node_chunks <- function(chunks) {

	n_flow <- unlist(lapply(chunks, function(x) x$n_flow))
	snap_check <- unlist(lapply(chunks, function(x) x$snap_check))
	unsnapped_tonodes <- NULL

	if (!is.null(chunks[[1]]$zero_pairs)) {
		chunk_rows <- vapply(chunks, function(x) length(x$n_flow), 1L)
		offsets <- cumsum(c(0L, chunk_rows[-length(chunk_rows)]))
		zero_pairs <- do.call(rbind, lapply(seq_along(chunks), function(i) {
			z <- chunks[[i]]$zero_pairs
			cbind(z[, 1] + offsets[i], z[, 2])
		}))
		dbl_tonodes <- which(tabulate(zero_pairs[, 2]) > 1)
		unsnapped_tonodes <- sort(unique(zero_pairs[zero_pairs[, 2] %in% dbl_tonodes, 1]))
	}

	return(list(n_flow = n_flow, snap_check = snap_check,
							unsnapped_tonodes = unsnapped_tonodes))
}
