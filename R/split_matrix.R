
split_matrix = function(mat, n) {
	
	n.mat = nrow(mat)
	chunk.size = ceiling(n.mat / n)
	ix = c(seq(from=1, to=n.mat, by=chunk.size), n.mat+1) 
	
	mat.list = list()
	for(i in 1:n) {
		if(ix[i] > n.mat) break
		mat.list[[i]] = mat[ix[i]:(ix[i+1]-1), , drop = FALSE]
	}
	
	return(mat.list)
}
