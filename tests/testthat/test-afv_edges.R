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
