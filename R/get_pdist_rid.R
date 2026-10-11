
get_pdist_rid<- function(from_xy, to_xy, ncores, snap_tolerance) {

	to.index<-NULL

	# 2.1 GB = 2.1 x 1024^3 = 2256452608
	# 8 bites per element
	# 2256452608/8 = 282031744 max elements
	## Check number of elements 282000000
	## 282,031,744 elements max for 2.1Gb

  max.to.rows <- floor(280000000/nrow(from_xy))/2
  if(max.to.rows > 500) max.to.rows <- 500

  chunk.no <- ceiling(nrow(to_xy)/max.to.rows)
  #if(chunk.no < 1) chunk.no <- 1

	# Split the to_xy into a list of matrices
	to_xy_list <- split_matrix(to_xy, chunk.no)

	cl <- makeCluster(ncores)
	registerDoParallel(cl)

	## close cluster upon function exit
	on.exit({
		stopCluster(cl)
		suppressWarnings(closeAllConnections())
	})

	# if (is.null(getDoParName())) {
	# 	registerDoSEQ()
	# }

	## Calculate distances, identify FC edges that share end node,
	## return as list
	rid_confl<- foreach(to.index = 1:length(to_xy_list),
											.combine = 'c',
											.inorder = TRUE,
											.packages = c("SSNbler"),
											.export = c("pdist_rid_confl"),
											.errorhandling = "stop") %dopar% {

												pdist_rid_confl(from_xy = from_xy,
																	to_xy = to_xy_list[[to.index]],
																	snap_tolerance = snap_tolerance)
											}

	return(rid_confl)

}
