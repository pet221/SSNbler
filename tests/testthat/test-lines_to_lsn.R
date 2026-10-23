test_that("lines_to_lsn", {
	# MF_streams
	expect_equal(dim(edges_lsn), c(163, 11))
	expect_true("rid" %in% names(edges_lsn))
	expect_equal(edges_lsn$rid[1:2], c(1, 2))
	expect_s3_class(edges_lsn, "sf")
})

# Synthetic cases for lines_to_lsn(), grid_pairs() and split_self_loops()

## Helpers ----

# Unordered pairs as a sorted (min, max) matrix
norm_pairs <- function(p) {
	p <- unname(cbind(pmin(p[, 1], p[, 2]), pmax(p[, 1], p[, 2])))
	p[order(p[, 1], p[, 2]), , drop = FALSE]
}

# All pairs <= tol apart, from the full distance matrix
brute_pairs <- function(xy, tol) {
	d <- as.matrix(stats::dist(xy))
	norm_pairs(which(d <= tol & upper.tri(d), arr.ind = TRUE))
}

# Pairs and igraph groups of line ends, as in lines_to_lsn()
group_ends <- function(end.xy, tol) {
	p <- grid_pairs(end.xy, tol)
	g <- igraph::make_empty_graph(n = nrow(end.xy), directed = FALSE)
	g <- igraph::add_edges(g, as.vector(t(p)))
	list(pairs = p, memb = igraph::components(g)$membership)
}

# Run lines_to_lsn() on a list of coordinate matrices and read the
# outputs back. The temporary LSN folder lasts until the calling test ends
grid_lsn <- function(coords, snap, topo = 20) {
	lines <- lapply(coords, sf::st_linestring)
	streams <- sf::st_sf(id = seq_along(lines),
		geometry = sf::st_sfc(lines, crs = 5070))
	lsn.path <- withr::local_tempdir(.local_envir = parent.frame())
	lines_to_lsn(streams, lsn_path = lsn.path, check_topology = TRUE,
		snap_tolerance = snap, topo_tolerance = topo, overwrite = TRUE,
		verbose = FALSE)

	rel <- utils::read.csv(file.path(lsn.path, "relationships.csv"))
	rel <- rel[order(rel$fromedge, rel$toedge), , drop = FALSE]
	rownames(rel) <- NULL
	err.file <- file.path(lsn.path, "node_errors.gpkg")
	list(nodecat = sf::st_read(file.path(lsn.path, "nodes.gpkg"), quiet = TRUE)$nodecat,
		noderel = utils::read.csv(file.path(lsn.path, "noderelationships.csv")),
		rel = rel,
		errors = if (file.exists(err.file)) {
			sort(sf::st_read(err.file, quiet = TRUE)$error)
		} else {
			character(0)
		})
}

x0 <- 1e6
y0 <- 2e6

## grid_pairs() ----

test_that("grid_pairs() matches brute force", {
	set.seed(8)
	# Around the origin: negative coordinates and cells either side of 0
	xy <- cbind(runif(400, -3, 3), runif(400, -3, 3))
	expect_equal(norm_pairs(grid_pairs(xy, 0.25)), brute_pairs(xy, 0.25))

	# Large coordinates (UTM northings) with a 1 mm tolerance
	xy <- cbind(5e5 + runif(400, 0, 0.05), 1e7 + runif(400, 0, 0.05))
	expect_equal(norm_pairs(grid_pairs(xy, 0.001)), brute_pairs(xy, 0.001))

	# Tight clusters, many points per cell
	ctr <- cbind(runif(20, 0, 1000), runif(20, 0, 1000))
	xy <- ctr[rep(1:20, each = 20), ] + runif(800, -0.005, 0.005)
	expect_equal(norm_pairs(grid_pairs(xy, 0.01)), brute_pairs(xy, 0.01))
})

test_that("grid_pairs() includes points exactly tol apart, across cell boundaries", {
	# Points on a 0.25 lattice (exact in binary): neighbours are exactly tol
	# apart horizontally and vertically, diagonals are not paired
	k <- -5:5
	xy <- as.matrix(expand.grid(x = k * 0.25, y = k * 0.25))
	p <- grid_pairs(xy, 0.25)
	expect_equal(nrow(p), 2 * 11 * 10)
	expect_equal(norm_pairs(p), brute_pairs(xy, 0.25))

	# Just above tol is not paired
	xy <- rbind(c(x0, y0), c(x0 + 0.25, y0), c(x0 + 0.5 + 2^-10, y0))
	expect_equal(norm_pairs(grid_pairs(xy, 0.25)), matrix(c(1L, 2L), 1))
})

test_that("grid_pairs() pairs a point at -0 with its neighbour cell", {
	# floor(-0 / h) is -0; the cell key must still match "0"
	xy <- rbind(c(-0.05, 0), c(-0, -0))
	expect_equal(nrow(grid_pairs(xy, 0.1)), 1)
})

test_that("grid_pairs() with tol = 0 returns exact matches only", {
	xy <- rbind(c(1, 1), c(1, 1), c(1, 1 + 1e-9), c(5, 5), c(1, 1))
	expect_equal(norm_pairs(grid_pairs(xy, 0)),
		matrix(c(1L, 1L, 2L, 2L, 5L, 5L), ncol = 2))
})

test_that("grid_pairs() returns a 0-row matrix when nothing is paired", {
	expect_equal(dim(grid_pairs(rbind(c(0, 0), c(10, 10)), 1)), c(0L, 2L))
	expect_equal(dim(grid_pairs(matrix(c(0, 0), nrow = 1), 1)), c(0L, 2L))
})

## split_self_loops() ----

test_that("split_self_loops() leaves groups without both ends of a line alone", {
	# Line 1 ends 0.1 from the start of line 2
	end.xy <- rbind(c(0, 0), c(10, 0), c(10.1, 0), c(20, 0))
	ge <- group_ends(end.xy, 0.3)
	res <- split_self_loops(ge$pairs, end.xy, ge$memb)
	expect_identical(res$memb, ge$memb)
	expect_length(res$blocked, 0)
})

test_that("split_self_loops() splits a hairpin and reports both ends", {
	# Line 1's ends are 0.1 apart
	end.xy <- rbind(c(0, 0), c(0.1, 0), c(50, 50), c(60, 60))
	ge <- group_ends(end.xy, 0.3)
	expect_equal(ge$memb[1], ge$memb[2])
	res <- split_self_loops(ge$pairs, end.xy, ge$memb)
	expect_false(res$memb[1] == res$memb[2])
	expect_equal(sort(res$blocked), c(1, 2))
})

test_that("split_self_loops() merges closest first and blocks the rest", {
	# Line 1's ends are 0.2 apart. Line 2's to end is 0.05 from line 1's
	# to end and 0.15 from its from end. The 0.05 merge is made; the others
	# would put both ends of line 1 in one node
	end.xy <- rbind(c(0, 0), c(0.2, 0), c(0.15, 50), c(0.15, 0))
	ge <- group_ends(end.xy, 0.3)
	res <- split_self_loops(ge$pairs, end.xy, ge$memb)
	expect_equal(res$memb[2], res$memb[4])
	expect_false(res$memb[1] == res$memb[2])
	expect_false(res$memb[3] %in% res$memb[c(1, 2, 4)])
	expect_equal(sort(res$blocked), c(1, 2, 4))
})

test_that("split_self_loops() leaves a line with identical ends as a self-loop", {
	# Line 1 is a ring starting where line 2 starts
	end.xy <- rbind(c(0, 0), c(0, 0), c(0, 0), c(10, 10))
	ge <- group_ends(end.xy, 0.3)
	res <- split_self_loops(ge$pairs, end.xy, ge$memb)
	expect_identical(res$memb, ge$memb)
	expect_equal(length(unique(res$memb[1:3])), 1)
	expect_length(res$blocked, 0)
})

## lines_to_lsn() ----

test_that("ends chained within tolerance form one node with no error", {
	# A-B and B-C are 0.2 apart, A-C 0.4 (> snap 0.3)
	lsn <- grid_lsn(list(
		rbind(c(x0 - 100, y0 + 50), c(x0, y0)),
		rbind(c(x0 + 100, y0 + 50), c(x0 + 0.2, y0)),
		rbind(c(x0 + 0.4, y0), c(x0 + 0.4, y0 - 100))), snap = 0.3)
	expect_equal(sort(lsn$nodecat), c("Confluence", "Outlet", "Source", "Source"))
	expect_equal(lsn$rel, data.frame(fromedge = 1:2, toedge = c(3L, 3L)))
	expect_length(lsn$errors, 0)
})

test_that("a gap of exactly snap_tolerance is snapped, just above is not", {
	streams_gap <- function(gap) {
		list(rbind(c(x0 - 100, y0), c(x0, y0)),
			rbind(c(x0 + gap, y0), c(x0 + gap + 100, y0)))
	}
	lsn <- grid_lsn(streams_gap(0.25), snap = 0.25)
	expect_equal(sort(lsn$nodecat), c("Outlet", "Pseudonode", "Source"))
	expect_equal(lsn$rel, data.frame(fromedge = 1L, toedge = 2L))
	expect_length(lsn$errors, 0)

	lsn <- grid_lsn(streams_gap(0.25 + 2^-10), snap = 0.25)
	expect_equal(sort(lsn$nodecat), c("Outlet", "Outlet", "Source", "Source"))
	expect_equal(nrow(lsn$rel), 0)
	expect_equal(lsn$errors, "Dangling Node")
})

test_that("snap_tolerance = 0 connects exact matches only", {
	lsn <- grid_lsn(list(rbind(c(x0 - 100, y0), c(x0, y0)),
		rbind(c(x0, y0), c(x0 + 100, y0))), snap = 0)
	expect_equal(lsn$rel, data.frame(fromedge = 1L, toedge = 2L))
	expect_length(lsn$errors, 0)

	lsn <- grid_lsn(list(rbind(c(x0 - 100, y0), c(x0, y0)),
		rbind(c(x0 + 2^-10, y0), c(x0 + 100, y0))), snap = 0)
	expect_equal(nrow(lsn$rel), 0)
	expect_equal(lsn$errors, "Dangling Node")
})

test_that("a hairpin keeps two nodes and is flagged as Unsnapped Node", {
	# Line 1 starts 0.1 from where it ends (< snap 0.3); line 2 starts
	# exactly where line 1 ends
	lsn <- grid_lsn(list(
		rbind(c(x0, y0), c(x0, y0 + 50), c(x0 + 0.1, y0)),
		rbind(c(x0 + 0.1, y0), c(x0 + 0.1, y0 - 100))), snap = 0.3)
	expect_false(lsn$noderel$fromnode[1] == lsn$noderel$tonode[1])
	expect_equal(lsn$rel, data.frame(fromedge = 1L, toedge = 2L))
	expect_equal(lsn$errors, c("Unsnapped Node", "Unsnapped Node"))
})

test_that("a short line longer than snap_tolerance is not collapsed", {
	# Line 2 is 0.109 m long (rid 437163 in the Boise data). The old
	# rounding grid (0.1 m for snap 0.01) put both its ends in one node
	a <- c(993630.952000, 1920364.25260)
	b <- c(993631.017174, 1920364.34019)
	lsn <- grid_lsn(list(rbind(a - c(100, 0), a), rbind(a, b),
		rbind(b, b + c(0, 100))), snap = 0.01)
	expect_equal(length(lsn$nodecat), 4)
	expect_false(lsn$noderel$fromnode[2] == lsn$noderel$tonode[2])
	expect_equal(lsn$rel, data.frame(fromedge = 1:2, toedge = 2:3))
	expect_length(lsn$errors, 0)
})

test_that("a line with identical ends is a Self-loop with no self-relationship", {
	lsn <- grid_lsn(list(rbind(c(x0, y0), c(x0 + 50, y0), c(x0 + 50, y0 + 50),
		c(x0, y0))), snap = 0.3)
	expect_equal(length(lsn$nodecat), 1)
	expect_equal(nrow(lsn$rel), 0)
	expect_equal(lsn$errors, "Self-loop")
})

test_that("a single line gives a Source and an Outlet", {
	lsn <- grid_lsn(list(rbind(c(x0, y0), c(x0 + 100, y0))), snap = 0.3)
	expect_equal(sort(lsn$nodecat), c("Outlet", "Source"))
	expect_equal(nrow(lsn$rel), 0)
	expect_length(lsn$errors, 0)
})

test_that("MF LSN has one downstream edge per non-outlet edge and no errors", {
	# helper.R ran lines_to_lsn() on MF_streams into path
	rel <- utils::read.csv(file.path(path, "relationships.csv"))
	nodes <- sf::st_read(file.path(path, "nodes.gpkg"), quiet = TRUE)
	expect_false(any(duplicated(rel$fromedge)))
	expect_equal(nrow(rel), nrow(MF_streams) - sum(nodes$nodecat == "Outlet"))
	expect_false(file.exists(file.path(path, "node_errors.gpkg")))
})

## Arguments and inputs ----

# sf object from a list of coordinate matrices
make_streams <- function(coords, crs = 5070, dim = "XY", geom.name = "geometry") {
	lines <- lapply(coords, sf::st_linestring, dim = dim)
	streams <- sf::st_sf(id = seq_along(lines), geometry = sf::st_sfc(lines, crs = crs))
	if (geom.name != "geometry") sf::st_geometry(streams) <- geom.name
	streams
}

# Two lines meeting exactly at (x0, y0)
two.lines <- list(rbind(c(x0 - 100, y0), c(x0, y0)),
	rbind(c(x0, y0), c(x0 + 100, y0)))

read_rel_df <- function(p) {
	rel <- utils::read.csv(file.path(p, "relationships.csv"))
	rel <- rel[order(rel$fromedge, rel$toedge), , drop = FALSE]
	rownames(rel) <- NULL
	rel
}

test_that("lines_to_lsn() input checks stop with informative errors", {
	p <- withr::local_tempdir()
	run <- function(streams, ...) {
		lines_to_lsn(streams, lsn_path = p, overwrite = TRUE,
			verbose = FALSE, ...)
	}
	expect_error(run(data.frame(a = 1)), "must be an sf object")

	empty <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(
		sf::st_linestring(two.lines[[1]]), sf::st_linestring(), crs = 5070))
	expect_error(run(empty), "empty geometry")

	expect_error(run(make_streams(list(rbind(c(-120, 45), c(-119.9, 45))),
		crs = 4326)), "geographic coordinates")

	multi <- sf::st_sf(id = 1, geometry = sf::st_sfc(
		sf::st_multilinestring(two.lines), crs = 5070))
	expect_error(run(multi), "sf::st_cast")

	pts <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_point(c(x0, y0)),
		crs = 5070))
	expect_error(run(pts), "must have LINESTRING geometry")

	for (d in c("XYZ", "XYM", "XYZM")) {
		extra <- if (d == "XYZM") 2 else 1
		zm <- make_streams(lapply(two.lines, function(m) {
			cbind(m, matrix(0, nrow(m), extra))
		}), dim = d)
		expect_error(run(zm, snap_tolerance = 1, topo_tolerance = 20),
			"XYZ, XYM or XYZM geometries are not supported")
	}

	streams <- make_streams(two.lines)
	expect_error(run(streams, snap_tolerance = -1, topo_tolerance = 20),
		"must be >= 0")
	expect_error(run(streams, snap_tolerance = 1, topo_tolerance = 1),
		"topo_tolerance must be > snap_tolerance")
	expect_error(run(streams, snap_tolerance = 150, topo_tolerance = 200),
		"snap_tolerance must be < than the length")
})

test_that("remove_ZM = TRUE drops Z, M and ZM dimensions", {
	for (d in c("XYZ", "XYM", "XYZM")) {
		extra <- if (d == "XYZM") 2 else 1
		streams <- make_streams(lapply(two.lines, function(m) {
			cbind(m, matrix(0, nrow(m), extra))
		}), dim = d)
		p <- withr::local_tempdir()
		out <- lines_to_lsn(streams, lsn_path = p, snap_tolerance = 1,
			topo_tolerance = 20, remove_ZM = TRUE, overwrite = TRUE,
			verbose = FALSE)
		expect_equal(colnames(sf::st_coordinates(out)), c("X", "Y", "L1"))
		expect_equal(read_rel_df(p), data.frame(fromedge = 1L, toedge = 2L))
	}
})

test_that("check_topology = FALSE writes the LSN without node_errors", {
	# The gap would be a Dangling Node; topo_tolerance <= snap_tolerance
	# is allowed when topology isn't checked
	streams <- make_streams(list(rbind(c(x0 - 100, y0), c(x0, y0)),
		rbind(c(x0 + 2, y0), c(x0 + 100, y0))))
	p <- withr::local_tempdir()
	lines_to_lsn(streams, lsn_path = p, check_topology = FALSE,
		snap_tolerance = 1, topo_tolerance = 0, overwrite = TRUE,
		verbose = FALSE)
	expect_true(all(file.exists(file.path(p, c("edges.gpkg", "nodes.gpkg",
		"nodexy.csv", "noderelationships.csv", "relationships.csv")))))
	expect_false(file.exists(file.path(p, "node_errors.gpkg")))
	expect_equal(nrow(read_rel_df(p)), 0)
})

test_that("overwrite = FALSE stops when the LSN already exists", {
	streams <- make_streams(two.lines)
	p <- withr::local_tempdir()
	lines_to_lsn(streams, lsn_path = p, snap_tolerance = 1,
		topo_tolerance = 20, overwrite = TRUE, verbose = FALSE)
	expect_error(lines_to_lsn(streams, lsn_path = p, snap_tolerance = 1,
		topo_tolerance = 20, overwrite = FALSE, verbose = FALSE),
		"edges.gpkg exists")
})

test_that("existing rid and fid columns follow overwrite", {
	p <- withr::local_tempdir()
	run <- function(streams, overwrite) {
		lines_to_lsn(streams, lsn_path = p, snap_tolerance = 1,
			topo_tolerance = 20, overwrite = overwrite, verbose = FALSE)
	}
	for (col in c("rid", "fid")) {
		streams <- make_streams(two.lines)
		streams[[col]] <- c(10L, 20L)
		expect_error(run(streams, FALSE), paste(col, "already exists"))
		out <- run(streams, TRUE)
		expect_equal(out$rid, 1:2)
		if (col == "fid") expect_false("fid" %in% names(out))
	}
	streams <- make_streams(two.lines)
	streams$RID <- c(10L, 20L)
	expect_error(run(streams, TRUE), "ignoring case")
})

test_that("geometry column name, CRS and input columns are kept", {
	rel <- list()
	for (g in c("geometry", "geom")) {
		streams <- make_streams(two.lines, geom.name = g)
		p <- withr::local_tempdir()
		out <- lines_to_lsn(streams, lsn_path = p, snap_tolerance = 1,
			topo_tolerance = 20, overwrite = TRUE, verbose = FALSE)
		expect_equal(attr(out, "sf_column"), g)
		expect_equal(out$id, 1:2)
		expect_equal(out$rid, 1:2)
		nodes <- sf::st_read(file.path(p, "nodes.gpkg"), quiet = TRUE)
		expect_true(sf::st_crs(nodes) == sf::st_crs(streams))
		rel[[g]] <- read_rel_df(p)
	}
	expect_equal(rel$geom, rel$geometry)
})

test_that("duplicate lines give Converging Node and Downstream Divergence", {
	# Lines 2 and 3 are identical
	down <- rbind(c(x0, y0), c(x0 + 100, y0))
	lsn <- grid_lsn(list(rbind(c(x0 - 100, y0), c(x0, y0)), down, down),
		snap = 0.3)
	expect_equal(lsn$rel, data.frame(fromedge = c(1L, 1L), toedge = 2:3))
	expect_equal(lsn$errors, c("Converging Node", "Downstream Divergence"))
})

test_that("separate networks each get their own outlet", {
	lsn <- grid_lsn(c(two.lines,
		lapply(two.lines, function(m) m + matrix(c(0, 1000), 2, 2, byrow = TRUE))),
		snap = 0.3)
	expect_equal(sort(lsn$nodecat),
		rep(c("Outlet", "Pseudonode", "Source"), each = 2))
	expect_equal(lsn$rel, data.frame(fromedge = c(1L, 3L), toedge = c(2L, 4L)))
	expect_length(lsn$errors, 0)
})

test_that("tolerances are in map units (US feet)", {
	# NAD83 / North Carolina (ftUS); a 0.5 ft gap with snap_tolerance = 1 ft
	streams <- make_streams(list(rbind(c(2e6 - 100, 6e5), c(2e6, 6e5)),
		rbind(c(2e6 + 0.5, 6e5), c(2e6 + 100, 6e5))), crs = 2264)
	p <- withr::local_tempdir()
	lines_to_lsn(streams, lsn_path = p, snap_tolerance = 1,
		topo_tolerance = 20, overwrite = TRUE, verbose = FALSE)
	expect_equal(read_rel_df(p), data.frame(fromedge = 1L, toedge = 2L))
	expect_false(file.exists(file.path(p, "node_errors.gpkg")))
})

test_that("overwrite = TRUE removes node_errors.gpkg from an earlier run", {
	p <- withr::local_tempdir()
	run <- function(gap) {
		lines_to_lsn(make_streams(list(rbind(c(x0 - 100, y0), c(x0, y0)),
			rbind(c(x0 + gap, y0), c(x0 + 100, y0)))), lsn_path = p,
			snap_tolerance = 0.3, topo_tolerance = 20, overwrite = TRUE,
			verbose = FALSE)
	}
	run(2)  # 2 m gap: Dangling Node
	expect_true(file.exists(file.path(p, "node_errors.gpkg")))
	run(0)  # fixed
	expect_false(file.exists(file.path(p, "node_errors.gpkg")))
})
