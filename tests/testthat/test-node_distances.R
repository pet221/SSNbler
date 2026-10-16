# Distances between line end nodes at projected coordinates (#8)

test_that("proxy::dist is exact at projected coordinates", {
	# Offsets are exact in binary, so the true distances are known exactly
	for (m in 10^(0:6)) {
		a <- cbind(m + c(0, 1, 2), 2 * m + c(0, 0, 0))
		b <- a + cbind(c(2^-10, 3 * 2^-7, 0), c(0, 4 * 2^-7, 0))
		d <- unclass(proxy::dist(a, b))
		expect_identical(as.vector(diag(d)), c(2^-10, 5 * 2^-7, 0))
	}
	# Identical points are exactly 0 apart
	p <- cbind(878000.123, 1261000.456)
	expect_identical(as.vector(unclass(proxy::dist(p, p))), 0)
})

test_that("proxy::dist matches the Euclidean formula at BC Albers coordinates", {
	set.seed(8)
	a <- cbind(runif(50, 2e5, 1.9e6), runif(50, 3e5, 1.7e6))
	b <- cbind(runif(30, 2e5, 1.9e6), runif(30, 3e5, 1.7e6))
	dx <- outer(a[, 1], b[, 1], "-")
	dy <- outer(a[, 2], b[, 2], "-")
	expected <- sqrt(dx * dx + dy * dy)

	d <- unclass(proxy::dist(a, b))
	expect_equal(dim(d), c(50L, 30L))
	expect_equal(as.vector(d), as.vector(expected), tolerance = 1e-12)
})

test_that("proxy::dist accepts the input shapes lines_to_lsn() passes", {
	a <- cbind(c(0, 3, 6), c(0, 4, 8))
	b <- cbind(c(0, 3), c(0, 4))
	expected <- matrix(c(0, 5, 10, 5, 0, 5), nrow = 3)

	# data frame (node_coords) and matrix
	d <- unclass(proxy::dist(as.matrix(as.data.frame(a)), as.matrix(b)))
	expect_equal(as.vector(d), as.vector(expected))
	# one-row matrix stays one point, not one column of points
	d.one <- unclass(proxy::dist(a[1, , drop = FALSE], b))
	expect_equal(dim(d.one), c(1L, 2L))
	expect_equal(as.vector(d.one), c(0, 5))
	# rownames are kept as dimnames (rid values)
	rownames(b) <- c("7", "9")
	d.names <- unclass(proxy::dist(a, b))
	expect_equal(colnames(d.names), c("7", "9"))
})

test_that("a 3 cm line at BC Albers coordinates gives no node errors", {
	# Three lines digitized east (downstream); the middle line is 3 cm long.
	# Every end node meets the next start node exactly.
	x0 <- 878000
	y0 <- 1261000
	xs <- x0 + c(0, 10, 10.03, 20.03)
	lines <- lapply(1:3, function(i) sf::st_linestring(cbind(xs[i + 0:1], y0)))
	streams <- sf::st_sf(id = 1:3, geometry = sf::st_sfc(lines, crs = 3005))

	lsn.path <- withr::local_tempdir()
	lines_to_lsn(streams, lsn_path = lsn.path, check_topology = TRUE,
		snap_tolerance = 0.001, topo_tolerance = 1, overwrite = TRUE,
		verbose = FALSE)

	expect_false(file.exists(file.path(lsn.path, "node_errors.gpkg")))

	nodes <- sf::st_read(file.path(lsn.path, "nodes.gpkg"), quiet = TRUE)
	expect_equal(sort(nodes$nodecat),
		sort(c("Source", "Pseudonode", "Pseudonode", "Outlet")))

	rel <- utils::read.csv(file.path(lsn.path, "relationships.csv"))
	rel <- rel[order(rel$fromedge), ]
	rownames(rel) <- NULL
	expect_equal(rel, data.frame(fromedge = c(1L, 2L), toedge = c(2L, 3L)))
})
