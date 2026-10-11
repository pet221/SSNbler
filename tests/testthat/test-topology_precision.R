# Topology checks at projected coordinates (pet221/SSNbler#8)

# A straight chain of n lines, 10 m each, digitized in the direction of flow (east),
# with line k replaced by a 3 cm line. Every end meets the next start exactly, so
# there are no topology errors.
chain <- function(n, k, x0, y0) {
	xs <- x0 + c(seq(0, by = 10, length.out = k),
		10 * (k - 1) + 0.03 + seq(0, by = 10, length.out = n - k + 1))
	l <- lapply(seq_len(n), function(i) sf::st_linestring(cbind(xs[i + 0:1], y0)))
	sf::st_sf(id = seq_len(n), geometry = sf::st_sfc(l, crs = 3005))
}

build_lsn <- function(lines, lsn_path, use_parallel = FALSE) {
	lines_to_lsn(lines, lsn_path = lsn_path, check_topology = TRUE,
		snap_tolerance = 0.001, topo_tolerance = 1, overwrite = TRUE,
		use_parallel = use_parallel, no_cores = if (use_parallel) 2 else NULL,
		verbose = FALSE)
	errors_file <- file.path(lsn_path, "node_errors.gpkg")
	list(
		errors = if (file.exists(errors_file)) sf::st_read(errors_file, quiet = TRUE) else NULL,
		nodecat = sf::st_read(file.path(lsn_path, "nodes.gpkg"), quiet = TRUE)$nodecat,
		relationships = utils::read.csv(file.path(lsn_path, "relationships.csv"))
	)
}

test_that("dist_xy is exact at projected coordinates", {
	# offsets exact in binary, so the true distance is known exactly
	for (m in 10^(0:6)) {
		a <- cbind(m + c(0, 1, 2), 2 * m + c(0, 0, 0))
		b <- a + cbind(c(2^-10, 3 * 2^-7, 0), c(0, 4 * 2^-7, 0))
		d <- dist_xy(a, b)
		expect_identical(diag(d), c(2^-10, 5 * 2^-7, 0))
	}
	# identical points are exactly 0
	p <- cbind(878000.123, 1261000.456)
	expect_identical(dist_xy(p, p), matrix(0, 1, 1))
})

test_that("dist_xy matches the definition for any shape and block size", {
	a <- cbind(c(0, 3, 6, 9, 12), c(0, 4, 8, 12, 16))
	b <- cbind(c(0, 3), c(0, 4))
	expected <- matrix(c(0, 5, 10, 15, 20, 5, 0, 5, 10, 15), nrow = 5)
	expect_identical(dist_xy(a, b), expected)
	expect_identical(dist_xy(b, a), t(expected))
	expect_identical(dist_xy(as.data.frame(a), b), expected)
	expect_identical(dist_xy(a[1, , drop = FALSE], b), expected[1, , drop = FALSE])
	# several blocks with a remainder
	expect_identical(dist_xy(a, b, block_size = 2), expected)
	expect_identical(dist_xy(a, b, block_size = 1), expected)
})

test_that("split_matrix keeps every chunk a matrix", {
	m <- matrix(seq_len(20), ncol = 2, dimnames = list(letters[1:10], NULL))
	chunks <- split_matrix(m, 4)
	expect_equal(vapply(chunks, nrow, 1L), c(3L, 3L, 3L, 1L))
	expect_true(all(vapply(chunks, is.matrix, TRUE)))
	expect_identical(rownames(chunks[[4]]), "j")
	expect_identical(do.call(rbind, chunks), m)
})

test_that("chunked node distances agree with the whole node matrix", {
	# 701 nodes split into two chunks of 351 and 350. Rows 351/352 share coordinates
	# across the chunk boundary, rows 600/601 within the second chunk. Neither can
	# happen through lines_to_lsn(), which deduplicates nodes, so the combiner is
	# tested directly.
	node_xy <- cbind(878000 + 10 * (0:700), 1261000)
	node_xy[352, ] <- node_xy[351, ]
	node_xy[601, ] <- node_xy[600, ]
	to_xy <- node_xy[-1, ] + cbind(c(0.0005, rep(0, 699)), 0)

	d <- dist_xy(node_xy, to_xy)
	dbl_tonodes <- which(colSums(d == 0) > 1)
	serial <- list(
		n_flow = apply(d, 1, function(x) sum(x <= 0.001)),
		snap_check = apply(d, 1, function(x) sum(x > 0 & x <= 0.001)),
		unsnapped_tonodes = which(rowSums(d[, dbl_tonodes, drop = FALSE] == 0) > 0)
	)
	expect_equal(serial$unsnapped_tonodes, c(351, 352, 600, 601))

	chunks <- lapply(split_matrix(node_xy, 2), pdist_node_coords,
		other_xy = to_xy, snap_tolerance = 0.001, node_dir = "to")
	expect_identical(combine_node_chunks(chunks), serial)
})

# 3 lines, the middle one 3 cm long. The coordinates are pinned: at (878000,
# 1261000) pdist::pdist() reads this 3 cm as 0, but it does so at only about half
# to two thirds of random positions in BC Albers, so a random one might not.
test_that("a few-cm line at BC Albers coordinates reports no node errors", {
	near_origin <- build_lsn(chain(3, 2, 0, 0), withr::local_tempdir())
	bc_albers <- build_lsn(chain(3, 2, 878000, 1261000), withr::local_tempdir())
	expected_rel <- data.frame(fromedge = c(1L, 2L), toedge = c(2L, 3L))

	for (lsn in list(near_origin, bc_albers)) {
		expect_null(lsn$errors)
		expect_equal(sort(table(lsn$nodecat)),
			sort(table(c("Source", "Pseudonode", "Pseudonode", "Outlet"))))
		rel <- lsn$relationships[order(lsn$relationships$fromedge), ]
		rownames(rel) <- NULL
		expect_equal(rel, expected_rel)
	}
})

test_that("the parallel path gives the same network as the serial path", {
	skip_on_cran()
	# 700 lines: 701 nodes, two chunks; the 3 cm line's nodes are rows 299/300 of the second
	lines <- chain(700, 650, 878000, 1261000)
	serial <- build_lsn(lines, withr::local_tempdir())
	parallel <- build_lsn(lines, withr::local_tempdir(), use_parallel = TRUE)

	expect_null(serial$errors)
	expect_null(parallel$errors)
	expect_identical(parallel$nodecat, serial$nodecat)
	expect_equal(parallel$relationships, serial$relationships)
	expect_equal(nrow(serial$relationships), 699)
})
