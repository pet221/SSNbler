test_that("afv_edges", {
	expect_equal(dim(edges_afv), c(163, 15))
	expect_true("areaPI" %in% names(edges_afv))
	expect_equal(edges_afv$areaPI[1:2], c(0.426, 1.000), tolerance = 0.001)
	expect_true(all(edges_afv$areaPI >= 0))
	expect_true(all(edges_afv$areaPI <= 1))
	expect_true("afvArea" %in% names(edges_afv))
	expect_equal(edges_afv$afvArea[1:2], c(0.171, 0.171), tolerance = 0.001)
	expect_true(all(edges_afv$afvArea >= 0))
	expect_true(all(edges_afv$afvArea <= 1))
	expect_true(all(edges_afv$areaPI >= edges_afv$afvArea))
	expect_s3_class(edges_afv, "sf")
})

test_that("afv_edges stops when a Self-loop is present", {
	# Inside a network (also a downstream divergence) and at the outlet
	# (otherwise silently wrong results)
	e <- self_loop_lsn("inside")
	expect_error(afv_edges(e, attr(e, "lsn_path"), infl_col = "area",
			segpi_col = "areaPI", afv_col = "afvArea", save_local = FALSE),
		"1 Self-loop edge\\(s\\) \\(rid 2\\)")
	e <- self_loop_lsn("outlet")
	expect_error(afv_edges(e, attr(e, "lsn_path"), infl_col = "area",
			segpi_col = "areaPI", afv_col = "afvArea", save_local = FALSE),
		"1 Self-loop edge\\(s\\) \\(rid 4\\)")
})

test_that("afv_edges stops when a network has no outlet", {
	# Lines 1-3 form a loop and line 4 flows into it; lines 5-6 are valid.
	# Nothing is calculated or written, so edges.gpkg keeps all 6 edges
	e <- no_outlet_lsn()
	expect_error(afv_edges(e, attr(e, "lsn_path"), infl_col = "area",
		segpi_col = "areaPI", afv_col = "afvArea", save_local = TRUE),
		"4 edge\\(s\\) \\(rid 1, 2, 3, 4\\) are in a network without an outlet")
	expect_equal(nrow(sf::st_read(file.path(attr(e, "lsn_path"), "edges.gpkg"),
		quiet = TRUE)), 6)})
