
## Euclidean distances between the rows of a and the rows of b (x in column 1, y in
## column 2), as a nrow(a) x nrow(b) matrix, in double precision. pdist::pdist()
## rounds coordinates to single precision before subtracting, so at projected
## coordinates two points several cm apart can read as 0 apart.
## Rows are filled block_size at a time, so the temporaries stay small.
dist_xy <- function(a, b, block_size = max(1, floor(5e6 / NROW(b)))) {

	if (is.null(dim(a))) a <- matrix(a, nrow = 1)
	if (is.null(dim(b))) b <- matrix(b, nrow = 1)
	a <- as.matrix(a)
	b <- as.matrix(b)

	out <- matrix(0, nrow = nrow(a), ncol = nrow(b))
	if (nrow(a) == 0 || nrow(b) == 0) return(out)

	bx <- b[, 1]
	by <- b[, 2]
	for (start in seq(1, nrow(a), by = block_size)) {
		i <- start:min(start + block_size - 1, nrow(a))
		dx <- outer(a[i, 1], bx, "-")
		dy <- outer(a[i, 2], by, "-")
		out[i, ] <- sqrt(dx * dx + dy * dy)
	}

	out
}
